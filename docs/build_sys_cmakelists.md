The CMakeLists
--------------
The build system consists of a tree of *CMakeLists.txt* files: The top-level *CMakeLists.txt* adds the *gw/* and *sw/* subdirectories. The *CMakeLists.txt* files in those subdirectories add the *components/* and *projects/* subdirectories, etc., down to the individual GW and SW component and project directories.

### A Gateware Component CMakeList

The build instructions for a gateware component are grouped into one CMake function: **gw_component_rules()**. A GW component-level CMakeLists.txt file contains just a call to this function, passing in the expected parameters:

```
gw_component_rules(
    TOP_MODULE <top module name> 
    COMPONENT_NAME <component name> 
)
```

For example:

```
gw_component_rules(
    TOP_MODULE wb_wbuart_wrap_wrap 
    COMPONENT_NAME wbuart32 
)
```

The component's sources, definitions, and dependencies are still defined in its *Bender.yml* manifest. The CMake build system interfaces with Bender through a set of scripts to extract the necessary info and pass it on to Vivado or Verilator.

### A Gateware Project CMakeList

The build instructions for a gateware project are also grouped into a CMake function: **gw_project_rules()**. This function has a few additional arguments compared to its component counterpart. A typical GW project CMakeLists.txt file looks like this:

```
gw_project_rules(
    TOP_MODULE <top module name>
    PROJECT_NAME <project name>
    MEM_FILE_TARGET <sw project name>
    VERILATOR_CPP_FLAGS <Verilator CPP flags, e.g. include paths>
    VERILATOR_LD_FLAGS <Verilator link flags, e.g. -lncurses>
)

#Add testcase.
add_test(NAME <test name>
    COMMAND <test command>
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
```

For example:

```
gw_project_rules(
    TOP_MODULE ibex_soc
    PROJECT_NAME hello_world
    MEM_FILE_TARGET hello_world
    VERILATOR_CPP_FLAGS "-I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
    VERILATOR_LD_FLAGS "-lncurses"
)

add_test(NAME hello_world_test
    COMMAND ./Vmodel
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
```

As is the case for GW components, the project's sources, definitions, dependencies, and constraint files are defined in its *bender.yml* manifest. The reference to the SW project delivering the memory file is *not* defined in the Bender manifest, however. The SW project name is passed in as the *MEM_FILE_TARGET* parameter in the *gw_project_rules()* call.

Any test cases are also added to the project's CMakeLists.txt file.

### Software Build Structure

CMake is designed to build software. The necessary functions for creating libraries, executables, etc. are predefined.
The only custom function added to the software CMakeLists tree is **link_internal_create_mem_file()**. This function implements the necessary instructions to link the given executable against the BoxLambda internal memory map and generate a memory file, to be used by the GW part of the build system.

The linker script to be executed is passed as an argument to the *link_internal_create_mem_file()* function. Currently, two linker scripts are defined:

- **/sw/components/bootstrap/link_internal_mem_64K.ld**: This is the linker script for an internal memory size of 64KB. This script results in a software image that can run both on the Arty-A7-35T and the Arty-A7-100T (FPGA and simulation).
- **/sw/components/bootstrap/link_internal_mem_256K.ld**: This is the linker script for an internal memory size of 256KB. This script results in a software image that only runs on the Arty-A7-100T (FPGA and simulation).

A typical SW project CMakeLists.txt file looks like this:

```
add_executable(hello_world
	EXCLUDE_FROM_ALL
    hello.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world
	PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(hello_world ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
```

### Organization

![CMakeLists organization.](assets/CMakeLists_Org.drawio.png)

*CMakeLists Organization.*

The actual gateware build recipes (Bender interaction, verilating, synthesizing...) are implemented by a set of bash and tcl scripts kept in the [scripts/](https://github.com/epsilon537/boxlambda/tree/master/scripts) directory:

```
	bender_gen_constraints_file_list.sh
	bender_gen_verilator_sources.sh
	bender_gen_vivado_sources.sh
	bender_get_cpp_files.sh
	bender_get_vlts.sh
	gen_mem_file_list.sh
	prg_bitstream.tcl
	verilator_lint_check.sh
	verilator_sim.sh
	vivado_create_project.tcl
	vivado_impl.tcl
	vivado_synth.tcl
    vivado_updatemem.sh
```

Having the build recipes as scripts instead of CMake allows me to invoke and test them outside of the build system.

The CMake build instructions define the various targets and the relationships between them and invoke the above build scripts when needed. 

The CMake build definitions are located as close as possible to the part of the tree to which they apply, e.g. the *gw_project_rules()* function can be found in the [gw/projects/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/CMakeLists.txt) file. *Gw_component_rules()* can be found in the [gw/components/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/components/CMakeLists.txt) file. Gateware build instructions common to both components and projects are located in the [gw/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/CMakeLists.txt) file.

### Cross-Compilation

RISC-V cross-compilation for C and C++ is set up by passing in a *toolchain file* to CMake. The toolchain file is located in [scripts/toolchain.cmake](https://github.com/epsilon537/boxlambda/blob/master/scripts/toolchain.cmake).

### Bender Interaction Hack

GNU Make, CMake's backend, uses the modification date of dependencies to decide if a build rule should be triggered, e.g. an object gets rebuilt when the corresponding source code has a more recent modification date than the object file itself. With Bender, however, a component's or project's *Bender.yml* file is just the tip of a tree. The Bender target and package dependencies also have to be considered. Simply listing the Bender.yml file as a dependency is not good enough. Instead, I'm using the Bender script output as a dependency:

1. The build system runs the *bender script* command.
2. The output of that command is stored in a temporary file. 
3. That file is compared with the Bender script output file used by the previous build of the same target. 
	- If it's different, the file is copied over, making it the Bender script output file to be used by the next build step. The Bender script output file is a dependency for synthesis, so synthesis will be triggered.
	- If the temporary file is the same as the Bender script output file used by the previous build of that target, the temporary file is discarded. Synthesis will not be triggered.

This mechanism is implemented in the [scripts/bender_gen_vivado_sources.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/bender_gen_vivado_sources.sh) and [scripts/bender_gen_verilator_sources.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/bender_gen_verilator_sources.sh) scripts. The same scripts also generate a *DepFile*: a dependency list of all the sources referenced in the Bender manifest. This DepFile is referenced by the synthesis target so synthesis (or verilation) will be triggered if any of the sources change.

![CMake and Bender Interaction.](assets/CMake_Bender_Interaction.drawio.png)

*CMake and Bender Interaction.*

### *.Picoasm* - CMake Build Support for PicoRV Assembly Code

Although Ibex and PicoRV32 are both 32-bit RISC-V processors, the programming model for the two is very different. For Ibex, we build and run C/C++ code, and use the **riscv-unknown-elf-gcc** frontend for compilation and linking. For PicoRV, we write tiny assembly programs. We use **riscv-unknown-elf-as** for assembling and **riscv-unknown-elf-ld** for linking. Separate CMake toolchain variables are set up to build PicoRV code. This is done by creating an **ASM_PICO** assembler *dialect* as described in this article from Kitware:

[https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/languages/Assembler](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/languages/Assembler)

The article tells you to create three files in the CMake modules directory (*cmake/* in the BoxLambda source tree). Here are the files I created for *ASM_PICO*:

[https://github.com/epsilon537/boxlambda/tree/master/cmake](https://github.com/epsilon537/boxlambda/tree/master/cmake)

A new filename extension is needed for PicoRV assembly files because CMake toolchain selection is done based on the filename extension. I decided to go for **.picoasm** for the source code and **.picobin** for the generated binaries.

*.Picoasm* assembly files use CPP as a preprocessor. CPP *#defines* allow the programmer to assign names to registers, making writing RISC-V assembly code more manageable:

```
#define hir_base x1
#define burst_base x2
#define msb_set x3
#define mask_4_lsb x4
#define src x5
#define dst x6
#define num_elems x7
#define burst_end x8
#define single_end x9
#define stat_busy x10
#define tmp x11
...
_start:
    /*Set up constants.*/
    li hir_base, HIR_REGS_BASE_ADDR
    li burst_base, BURST_REGS_BASE_ADDR
    sw zero, BURST_OFFSET(burst_base)    /*no src-to-dest alignment offset.*/
    li msb_set, 0x80000000
    li mask_4_lsb, 0xfffffff0            /*mask to clear 4 lsbs*/
    li stat_busy, STAT_BUSY
wait_start:
    lw tmp, HIR3(hir_base)               /*HIR3: ctrl-status*/
    beqz tmp, wait_start
...
```