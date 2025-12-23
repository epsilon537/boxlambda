---
layout: post
title: 'Building Software and Gateware with CMake and Bender.'
comments: true
---

*Updated 23 December 2025:*
- *Corrected link to Bender in documentation.*
- *Removed reference to 'On WSL' documentation.*

Recap
-----
This is a summary of the current state of affairs for BoxLambda. We have:
- An Ibex RISC-V core, a Wishbone shared bus, a Debug Core, internal memory, a timer, two GPIO ports, and a UART core.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- Test builds running on Arty-A7-35T and Verilator.
- Automated testing on Verilator.
- OpenOCD-based Debug Access, both on FPGA and Verilator.
- A Linux GNU Makefile and Bender-based RTL build system.
- DDR3 external memory access through the Litex Memory Controller.

The Case for CMake
------------------
Around the time I integrated [Picolibc into BoxLambda](https://epsilon537.github.io/boxlambda/libc-for-boxlambda/) it became clear that I needed to invest a bit more energy in BoxLambda's build system. The build system was based on a simple set of GNU Make rules and Bender manifests. It worked reasonably well for Verilator and FPGA synthesis, but there were a few limitations that were starting to hurt. Specifically, I wanted to add the following features to the build system:

- **Proper dependency tracking for software**: The old build system used forced build rules. Every software build was a full rebuild. The improved build system should use incremental builds with proper dependency tracking from memory files to executables, to libraries, and sources.
- **Proper dependency tracking for RTL**: Implementation and bitstream generation depends on synthesis. Synthesis depends on HDL sources, constraints files, and memory files. Memory files depend on software.
- **Out-of-Tree build trees**: Out-of-Tree build trees are way more convenient than In-Tree build trees. You can create as many of them as you want, they're easy to remove when you no longer need them, and the derived objects don't clutter up your source tree.
- **Support for build options**: I want to be able to specify whether the build tree is to be used for simulation builds, or for FPGA synthesis.
- **Support for different FPGA targets**: Arty-A7-35T and Arty-A7-100T to begin with.

I thought this would be a good opportunity to try out [Meson](https://mesonbuild.com/), a modern build system generator with a clean, elegant python-like syntax. The Meson experiment came to a halt pretty quickly, however. I just couldn't get my head around the fact that the Meson DSL does not include functions or macros. I ended up with a bunch of virtually identical *meson.build* files because I didn't have a way to abstract common patterns. You can take a look at BoxLambda's [meson branch](https://github.com/epsilon537/boxlambda/tree/meson) if you're interested.

I decided to switch over to [CMake](https://cmake.org/). Compared to Meson, CMake has a more cluttered, messy syntax, but it is more flexible. It has functions, macros, and all the other goodies you can expect of a build system generator:

- Easy dependency tracking.
- Support for Out-of-Tree builds.
- Support for build options.
- Support for automated testing.
- Configure-Time source code generation.

This is the first time I'm using a build system generator. I always got by with GNU Make itself. Now that I that I tried CMake, I have to say that I like it a lot. It's almost like going from assembly language to C. I probably could have implemented all the build system features I wanted directly in GNU Make, but it's so much easier in CMake.

Sidenote: Gateware
------------------
I first encountered the term *Gateware* in the [LiteX project](https://github.com/enjoy-digital/litex).

[Gateware comprises the description (of behaviour, structure and/or connections) of digital logic gates, a high level abstraction thereof, and/or the implementation thereof in (re)configurable logic devices (such as FPGAs and ASICs)](https://www.gateware.org/definition-of-gateware).

I think the term covers its meaning very well and it's the perfect counterpart for software. I'll be using it from here on out.

The Directory Structure
-----------------------
```
<BoxLambda Root Directory>
├── boxlambda_setup.sh
├── CMakeLists.txt
├── sub/
│   └── <git submodules>
├── gw/
│   ├── CMakeLists.txt
│   ├── components/
│   │   ├── CMakeLists.txt
│   │   ├── wbuart32/
│   │   │   ├── rtl/
│   │   │   ├── CMakeLists.txt
│   │   │   └── Bender.yml
│   │   └── <other gw component directories>
│   └── projects/
│       ├── CMakeLists.txt
│       ├── ddr_test/
│       │   ├── constr/
│       │   ├── rtl/
│       │   ├── sim/
│       │   ├── CMakeLists.txt
│       │   └── Bender.yml
│       └── <other gw project directories>
├── sw/
│   ├── CMakeLists.txt
│   ├── components/
│   │   ├── CMakeLists.txt
│   │   └── <sw component directories>
│   └── projects/
│       ├── CMakeLists.txt
│       ├── ddr_test/
│       │   ├── CMakeLists.txt
│       │   └── <ddr_test sources>
│       └── <other sw project directories>
└── build/
    ├── sim/
    ├── arty-a7-35/
    └── arty-a7-100/

```
I made some changes to BoxLambda's directory structure:

- I separated the software tree (*sw/*) from the gateware tree (*gw/*).
- The build trees (*build/*) are separate from the sw and gw source trees

The build trees are CMake build trees generated by the *boxlambda_setup.sh* script. As you can see in the tree diagram above, the script generates three build trees: one for simulation, one for the Arty-A7-35T, and one for the Arty A7-100T. The build trees are not under version control.

Building Gateware
-----------------
Assuming all [Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/) are installed, navigate to the build tree of choice and type:

```
make <gw component or project name>_<action>
```
where *action* is one of the following:

- **lint**: Lint-check the given gateware component or project.
- **sim**: Build the Verilator simulation model (*Vmodel*) of the given gateware project. This action only exists in the *sim/* build tree.
- **synth**: Synthesize the given gateware component or project. This action only exists in the *arty-a7* build trees.
- **impl**: Implement the given gateware project and generate its bitstream. This action only exists in the *arty-a7* build trees.
- **load**: Load the gateware project's bitstream file onto the connected target. This action only exists in the *arty-a7* build trees.

Some examples:
```
cd <boxlambda_root_dir>/build/sim && make ddr_test_sim
```
```
cd <boxlambda_root_dir>/build/arty-a7-35 && make hello_world_synth
```
```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_dbg_impl && make hello_dbg_load
```

The build directory tree mimics the source tree. When a build has been completed, a gateware project's Verilator model or the Vivado project files can be found under that project's directory. E.g.:
```
$ cd build/arty-a7-35/gw/projects/hello_world
$ make hello_world_synth
...
$ ls
CMakeFiles           hello_world.constraints_file_list  project.cache          project.runs    syn_util.rpt
CTestTestfile.cmake  hello_world.mem_file_list          project.dep            project.xpr
Makefile             hello_world.vivado_sources         project.hw             spram.mem
cmake_install.cmake  hello_world.vivado_sources.dep     project.ip_user_files  syn_timing.rpt
```

What happens when you run *make hello_world_synth*
==================================================
When you run *make hello_world_synth*, the following happens:

1. Make determines if (re)synthesis is needed. If synthesis is up-to-date, no further action is taken.
1. Make runs a *bender script* command on the bender.yml file in the *gw/projects/hello_world/* directory. The *bender script* command is wrapped in the *scripts/bender_gen_vivado_source.sh* shell script.
2. The bender script command processes that bender.yml manifest, as well as the bender.yml manifests of any dependent components.
3. The bender script command emits a list of all the HDL sources that make up the project.
4. Similarly, the *scripts/bender_gen_constraints_file_list.sh* and *scripts/bender_gen_mem_file_list.sh* emits the .xdc constraints and .mem memory files for the project.
5. Make feeds these file lists into a *vivado_create_project.tcl* script.
6. The *vivado_create_project.tcl* script creates a Vivado project.
7. Make kicks off the *vivado_synth.tcl* script which opens the Vivado project and starts synthesis.

When you run *make hello_world_impl*, the following happens:

1. Make determines if (re)implementation and bitstream generation is needed. If the bitstream file is up-to-date, no further action is taken. Make will also run the *hello_world_synth* rule above because it's a dependency of *hello_world_impl*.
2. Make kicks off the *vivado_impl.tcl* script which opens the Vivado project, picks up the synthesis checkpoint, and starts implementation.

See the BoxLambda documentation [Bender section](https://boxlambda.readthedocs.io/en/latest/build_sys_gw_build_struct/#bender) of for more info on how BoxLambda uses Bender.

Building Software
-----------------
The software corresponding with a gateware project automatically gets compiled, converted to a memory file, and included in the gateware project as part of the build process. Software projects can also be built independently. From the build directory just type: `make <sw project name>`. For example:

 ```
 $ cd sim/sw/projects/hello_world/
 $ make hello_world
...
$ ls
CMakeFiles           Makefile             hello_world      hello_world.map
CTestTestfile.cmake  cmake_install.cmake  hello_world.hex  hello_world.mem
 ```

Make All, Clean, and Regen
--------------------------
`make all` will lint check and build 'impl' all gateware projects.

`make clean` in a build tree will remove all the generated files that the build system is aware of. The generated files the build system is not aware of, e.g. synthesis utilization report files, will not be removed, however. If you want to go back to a completely clean build tree, type `make regen` from the build directory. This command will completely remove and regenerate the build tree.

Creating additional build trees
-------------------------------
You can easily create additional build trees from the BoxLambda root directory with the following command:

```
cmake --preset=sim|arty-a7-35|arty-a7-100 -B <build directory>
```

For example:

```
cmake --preset=sim -B build/sim2
```

Running Regression Tests
------------------------
CMake comes with a regression test framework called **Ctest**. BoxLambda regression tests are only defined in a simulation build tree. To see a list of available test cases, you need to first build everything and then run a `ctest -N` command to list the test cases:

```
cd <boxlambda root dir>/build/sim
make all
ctest -N
```

You should see something like this:

```
Test project /home/epsilon/work/boxlambda/build/sim2
  Test #1: hello_world_test
  Test #2: hello_dbg_test
  Test #3: picolibc_test_test
  Test #4: ddr_test_test

Total Tests: 4
```

To run a specific test, run the following command from the build directory:

```
ctest -I <test number>
```

To run all tests, just run the ctest command without any parameters.

The CMakeLists
--------------
The build system consists of a tree of *CMakeLists.txt* files: The top-level *CMakeLists.txt* adds the *gw/* and *sw/* subdirectories. The *CMakeLists.txt* files in those subdirectories add the *components/* and *projects/* subdirectories, etc., down to the individual GW and SW component and project directories.

A Gateware Component CMakeList
==============================
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

The component's sources, definitions, and dependencies are still defined in its *bender.yml* manifest. The CMake build system interfaces with Bender through a set of scripts to extract the necessary info and pass it on to Vivado or Verilator.

See the BoxLambda documentation [Bender section](https://boxlambda.readthedocs.io/en/latest/build_sys_gw_build_struct/#bender) of for more info on how BoxLambda uses Bender.

A Gateware Project CMakeList
============================
The build instructions for a gateware project are also grouped into a CMake function: **gw_project_rules()**. This function has a few additional arguments compared to its component counterpart. A typical GW project CMakeLists.txt file looks like this:

```
gw_project_rules(
    TOP_MODULE <top module name>
    PROJECT_NAME <project name>
    MEM_FILE_TARGET <sw project name>
    MEM_FILE_OUT <name of memory file expected by the SoC build. Currently, all project builds use spram.mem.>
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
    MEM_FILE_OUT spram.mem
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

Software Build Structure
========================
CMake is designed to build software. The necessary functions for creating libraries, executables, etc. are predefined.
The only custom function added to the software CMakeLists tree is **link_internal_create_mem_file()**. This function implements the necessary instructions to link the given executable against the BoxLambda internal memory map and generate a memory file, to be used by the GW part of the build system.

A typical SW project CMakeLists.txt file looks like this:

```
add_executable(hello_world
    EXCLUDE_FROM_ALL
    ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.c
    ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
    ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_compile_options(hello_world
	PRIVATE -g)

link_internal_create_mem_file(hello_world)
```

Implementation
--------------

![CMakeLists organization.](../assets/CMakeLists_Org.drawio.png)

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
```

Having the build recipes as scripts instead of CMake allows me to invoke and test them outside of the build system.

The CMake build instructions define the various targets and the relationships between them, and invoke the above build scripts when needed.

The CMake build definitions are located as close as possible to the part of the tree to which they apply, e.g. the *gw_project_rules()* function can be found in the [gw/projects/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/CMakeLists.txt) file. *Gw_component_rules()* can be found in the [gw/components/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/components/CMakeLists.txt) file. Gateware build instructions common to components and projects are located in the [gw/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/CMakeLists.txt) file.

Cross-Compilation
=================
RISC-V cross-compilation is set up by passing in a *toolchain file* to CMake. The toolchain file is located in [scripts/toolchain.cmake](https://github.com/epsilon537/boxlambda/blob/master/scripts/toolchain.cmake).

Picolibc GCC specs file
=======================
The Picolibc GCC specs file expects absolute paths. I'm using CMake's *configure_file()* to replace placeholders
in [scripts/picolibc.specs.in](https://github.com/epsilon537/boxlambda/blob/master/scripts/picolibc.specs.in) with the project source directory's absolute path. The resulting *picolibc.specs* is written in the root of the build tree. This way, the Picolibc library build for BoxLambda can be checked into the source tree and the user won't need to build and install it from source when setting up BoxLambda.

Bender Interaction Hack
=======================
GNU Make, CMake's backend, uses the modification date of dependencies to decide if a build rule should be triggered, e.g. an object gets rebuilt when the corresponding source code has a more recent modification date than the object file itself. With Bender, however, a component's or project's *bender.yml* file is just the tip of a tree. The Bender target and package dependencies also have to be considered. Simply listing the bender.yml file as a dependency is not good enough. Instead, I'm using the Bender script output as a dependency:

1. The build system runs the *bender script* command.
2. The output of that command is stored in a temporary file.
3. That file is compared with the Bender script output file used by the previous build of the same target.
	- If it's different, the file is copied over, making it the Bender script output file to be used by the next build step. The Bender script output file is a dependency for synthesis, so synthesis will be triggered.
	- If the temporary file is the same as the Bender script output file used by the previous build of that target, the temporary file is discarded. Synthesis will not be triggered.

This mechanism is implemented in the [scripts/bender_gen_vivado_sources.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/bender_gen_vivado_sources.sh) and [scripts/bender_gen_verilator_sources.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/bender_gen_verilator_sources.sh) scripts. The same scripts also generate a *DepFile*: a dependency list of all the sources referenced in the Bender manifest. This DepFile is referenced by the synthesis target so synthesis (or verilation) will be triggered if any of the sources change.

![CMake and Bender Interaction.](../assets/CMake_Bender_Interaction.drawio.png)

*CMake and Bender Interaction.*

I ran into a [minor Bender issue](https://github.com/pulp-platform/bender/issues/116) while testing this: When running the *bender script* command on the same bender.yml file twice, it would produce slightly different output, just a reordering of some lines, but enough to trip up the compare step. The Bender maintainer was very responsive and already fixed the issue. It's important to install Bender version 0.25.1 (or later) to get the fix.

Boxlambda_setup.sh
------------------
`make setup` has been replaced with the [boxlambda_setup.sh](https://github.com/epsilon537/boxlambda/blob/master/boxlambda_setup.sh) script in the repository root directory. The script initializes the git submodules used and creates the default build trees (*build/sim/*, *build/arty-a7-35/*, and *build/arty-a7-100/*).

*Make setup* also used to build the Picolibc library for BoxLambda. As said in the previous section, that is no longer needed. The compiled library is [checked into the source tree](https://github.com/epsilon537/boxlambda/tree/master/sw/picolibc-install).

Try It Out
----------
Below are the steps needed to set up the BoxLambda repository and build the **ddr_test** project on Verilator and Arty A7.
The build steps for test projects *hello_world*, *hello_dbg* and *picolibc_test* are analogous.

Repository setup
================
   1. Install the [Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
   2. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
   1. Switch to the *boxlambda_cmake* tag:
```
git checkout boxlambda_cmake
```
   1. Set up the repository. This initializes the git submodules used and creates the default build trees:
```
./boxlambda_setup.sh
```

Build and Run the DDR Test Image on Verilator
=============================================
   1. Build the ddr_test project:
```
cd build/sim/gw/projects/ddr_test
make ddr_test_sim
```
   2. Execute the generated verilator model in interactive mode:
```
./Vmodel -i
```
   3. You should see something like this:

![ddr_test on Verilator](../assets/ddr_test_verilator.png)

*DDR Test on Verilator.*

Build and Run the DDR Test Image on Arty A7
===========================================
   1. Build the ddr_test project:
```
cd build/arty-a7-[35|100]/gw/projects/ddr_test
make ddr_test_impl
```
   2. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.
   3. Load the bitstream onto the target board:
```
make ddr_test_load
```
   4. Verify the test program's output in the terminal. You should see something like this:

![ddr_test on Arty - Putty Terminal](../assets/ddr_test_arty.png)

*DDR Test on Arty A7.*

Interesting Links
-----------------
[https://github.com/BrunoLevy/learn-fpga#from-blinky-to-risc-v](https://github.com/BrunoLevy/learn-fpga#from-blinky-to-risc-v): This is a great two-part tutorial from Bruno Levy about implementing your own RISC-V processor.
