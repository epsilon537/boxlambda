---
hide:
  - toc
---

# The CMakeLists

The build system consists of a tree of `CMakeLists.txt` files. The top-level `CMakeLists.txt` adds the `gw/` and `sw/` subdirectories. The `CMakeLists.txt` files in those subdirectories add the `components/` and `projects/` subdirectories, etc., down to the individual GW and SW component and project directories.

## A Gateware Component CMakeList

The build instructions for a gateware component are grouped into one CMake function: `gw_component_rules()`. A GW component-level CMakeLists.txt file contains just a call to this function, passing in the expected parameters:

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

The component's sources, Vivado IPs, flags, and dependencies are defined in its `Bender.yml` manifest. The CMake build system interfaces with Bender through a collection of scripts to extract the necessary info and pass it on to Vivado or Verilator.

### DFX Components

DFX Components require additional build rules. A DFX Reconfigurable Module needs to be implemented and turned into a bitstream that can be dynamically loaded onto a DFX-enabled system. The build rules to do that are defined in the `gw_rm_rules_dfx()` function. Calling this function results in a `<component>_bit` target being defined. Building that target results in a DFX partial bitstream of the given component.

Here is an example:

```
gw_component_rules(
    TOP_MODULE  vs0
    COMPONENT_NAME vs0_j1b
)

if(CMAKE_BUILD_TYPE STREQUAL "fpga")
if(BL_TARGET_FPGA STREQUAL "arty-a7-100")
  gw_rm_rules_dfx(
    COMPONENT_NAMES
      vs0_j1b
    VS_INSTS
      boxlambda_soc_inst/GENERATE_VS0_MODULE.vs0_inst
    REF_DFX_PROJECT
      dfx_test
  )
endif()
```

The `gw_rm_rules_dfx()` parameters:

- `COMPONENT_NAMES`: The name of the component. If there are multiple RMs in the build, list the names of all RMs starting with the current component.
- `VS_INSTS`: Specify where in the SoC this component plugs into. If there are multiple RMs in the build, list the instance names in the same order as the component names list.
- `REF_DFX_PROJECT`: Specify the reference project defining the static portion of the DFX build.

## A Gateware Project CMakeList

The build instructions for a gateware project are also grouped into two CMake functions:

- `gw_project_rules_vivado()`: Gateware project build rules for Vivado builds.
- `gw_project_rules_verilator()`: Gateware project build rules for Verilator builds.

A typical GW project CMakeLists.txt file looks like this:

```
if(CMAKE_BUILD_TYPE STREQUAL "fpga")
  gw_project_rules_vivado(
      TOP_MODULE boxlambda_top
      PROJECT_NAME fatfs_test
      MEM_FILE_TARGET fatfs_test
  )
else()
  gw_project_rules_verilator(
      TOP_MODULE sim_main
      PROJECT_NAME fatfs_test
      MEM_FILE_TARGET fatfs_test
      VERILATOR_FLAGS
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/wbuart32/bench/cpp/"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/riscv-dbg/tb/remote_bitbang"
          "-CFLAGS -I${PROJECT_SOURCE_DIR}/sub/sdspi/bench/cpp"
  )

  # Add testcase.
  add_test(NAME fatfs_test_test
      COMMAND ./Vmodel -s ${CMAKE_CURRENT_LIST_DIR}/test/sdcard.img
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endif()
```

The project's sources, flags, dependencies, Vivado IPs, and constraint files are defined in its `Bender.yml` manifest. The reference to the SW project delivering the memory file is *not* defined in the Bender manifest, however. The SW project name is passed in as the `MEM_FILE_TARGET` parameter in the `gw_project_rules_vivado|verilator()` call.

Any test cases are also added to the project's `CMakeLists.txt` file.

## A DFX Project CMakeList

The build rules for a DFX-enabled project are created by the `gw_project_rules_dfx_vivado()` CMake function. Calling this function results in the creation of a `<project>_bit`, a `<project>_load`, and a `<project>_flash_gw` target. Building the `<project>_bit` target results in a bitstream file that can be `_loaded` or `_gw_flashed` onto the target. The gateware image expects to find a software image in flash memory to boot from.

Here is an example:

```
gw_project_rules_dfx_vivado(
    TOP_MODULE boxlambda_top
    PROJECT_NAME dfx_test
    VS_INSTS
      boxlambda_soc_inst/GENERATE_VS0_MODULE.vs0_inst
    VS_DEFAULT_COMPONENTS
      vs0_stub
)
```

The `gw_project_rules_dfx_vivado()` parameters:

- `TOP_MODULE`: Name of the top module.
- `PROJECT_NAME`: Project name.
- `VS_INSTS`: DFX virtual socket instance names.
- `VS_DEFAULT_COMPONENTS`: DFX virtual socket default components, one for each `VS_INST` listed. The default component gets placed into the virtual socket in the default bitstream image.

## A Software Project CMakeList

CMake is designed to build software. The necessary functions for creating libraries, executables, etc., are predefined. The only custom function added to the software CMakeLists tree is `link_and_create_image()`. This function executes the necessary steps to link the given target using a given linker script and generate a memory file, which is used by the GW part of the build system.

Currently, two linker scripts are defined:

- `/sw/components/bootstrap/link_imem_boot.ld`: This linker script creates software images that boot from IMEM internal memory.
- `/sw/components/bootstrap/link_flash_boot.ld`: This linker script creates software images that boot from flash memory.

A typical SW project `CMakeLists.txt` file looks like this:

```
#
# Hello World RAM Build
#

add_executable(hello_world_ram
 EXCLUDE_FROM_ALL
    hello.c
)

# Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world_ram
 PRIVATE -g)

# Function defined in parent CMakeLists.txt file:
link_and_create_image(hello_world_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(hello_world_ram gpio riscv)

#
# Hello World Flash Build
#

add_executable(hello_world_flsh
 EXCLUDE_FROM_ALL
    hello.c
)

# Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world_flsh
 PRIVATE -g)

# Function defined in parent CMakeLists.txt file:
link_and_create_image(hello_world_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(hello_world_flsh gpio riscv)
```

## CMakeList Organization

![CMakeLists Organization.](assets/CMakeLists_Org.png)

*CMakeLists Organization.*

The actual gateware build recipes (Bender interaction, verilating, synthesizing...) are implemented by a set of bash and tcl scripts kept in the [scripts/](https://github.com/epsilon537/boxlambda/tree/master/scripts) directory:

```
    bender_gen_prj_constraints_file_list.sh
    bender_gen_verilator_sources.sh
    bender_gen_vivado_sources_and_deps.sh
    bender_get_cpp_files.sh
    bender_get_dfx_constraints.sh
    bender_get_vlts.sh
    bender_gen_mem_file_list.sh
    bender_gen_ip_file_list.sh
    prg_bitstream.tcl
    verilator_lint_check.sh
    verilator_sim.sh
    vivado_create_project.tcl
    vivado_impl.tcl
    vivado_impl_dfx_prj.tcl
    vivado_impl_dfx_rm.tcl
    vivado_synth.tcl
    vivado_updatemem.sh
    ...
```

The build recipes are implemented as separate scripts outside the CMakeLists so they can be invoked from a Linux shell or Vivado session outside the build system.

The CMake build instructions define the various targets and the relationships between them and invoke the above build scripts when needed.

The CMake build definitions are located as close as possible to the part of the tree to which they apply, e.g., the `gw_project_rules()` function can be found in the [gw/projects/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/CMakeLists.txt) file. `Gw_component_rules()` can be found in the [gw/components/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/components/CMakeLists.txt) file. Gateware build instructions common to both components and projects are located in the [gw/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/gw/CMakeLists.txt) file.

