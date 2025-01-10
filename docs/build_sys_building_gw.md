# Building Gateware

Assuming all [Prerequisites](prerequisites.md) are installed, navigate to the desired build tree and run the following command:

```
make <gw_component_or_project_name><action>
```

Where *action* is one of the following:

- **_lint**: Lint-check the specified gateware component or project.
    - Depends on: Gateware sources.
    - Build tree: *sim-a7-\** build trees. Gateware component and project directories.
- **_sim**: Build the Verilator simulation model (*Vmodel*) for the specified gateware project. Note that only the gateware is built; the software project running on top of this gateware is not. See the *_sim_sw* target below for more details.
    - Depends on: Gateware sources.
    - Build tree: *sim-a7-\** build trees. Gateware project directories only.
- **_sim_sw**: Build the Verilator simulation model (*Vmodel*) for the specified gateware project and its associated software project. The SW image memory file is copied into the appropriate location in the gateware build directory to be picked up when the Verilator model executes.
    - Depends on: The *_sim* target above and the associated software target.
    - Build tree: *sim-a7-\** build trees. Gateware project directories only.
- **_synth**: Synthesize the specified gateware component or project. This action is only available in the *arty-a7-\** build trees. When you run the synth action on a component, Out-Of-Context (OOC) synthesis is performed, meaning the component's input and output ports are not connected to anything.
    - Depends on: Gateware sources and constraints.
    - Build tree: *arty-a7-\** build trees. Gateware component and project directories.
- **_bit**: Implement the specified gateware project and generate its bitstream. Note that only the gateware is built; the software project running on top of this gateware is not. The internal memory reserved for the software program will be left empty. See the *bit_sw* action below for more details.
    - Depends on: The *_synth* target above.
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_bit_sw**: Build the specified gateware project and its associated software project. The generated software image memory file is merged into the FPGA bitstream file. See [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).
    - Depends on: The *_bit* target above and the associated software target.
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_load**: Load the gateware project's bitstream file onto the connected target.
    - Depends on: No dependencies.
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_flash_gw**: Flash the gateware project's bitstream file onto the connected target.
    - Depends on: No dependencies.
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.

### Some Examples

```
cd <boxlambda_root_dir>/build/sim-a7-100 && make ddr_test_sim_sw
```

```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_world_synth
```

```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_dbg_bit_sw && make hello_dbg_load
```

The build directory tree mirrors the source tree. After a build completes, you can find the gateware project's Verilator model or Vivado project files under the corresponding project directory. For example:

```
$ cd build/arty-a7-100/gw/projects/hello_world
$ make hello_world_synth
...
$ ls
CMakeFiles                hello_world.constraints_file_list  project.cache        project.runs    syn_util.rpt
CTestTestfile.cmake       hello_world.mem_file_list          project.dep          project.xpr
Makefile                  hello_world.vivado_sources         project.hw           spram.mem
cmake_install.cmake       hello_world.vivado_sources.dep     project.ip_user_files syn_timing.rpt
```

### What Happens When You Run *make hello_world_(synth|bit|bit_sw)*?

![Gateware Build Targets.](assets/gw_build_targets.drawio.png)

*Gateware Build Targets.*

When you run *make hello_world_synth*, the following steps occur:

1. `make` checks if synthesis is up-to-date. If synthesis is already current, no further action is taken.
2. `make` runs the *bender script* command on the `bender.yml` file in the *gw/projects/hello_world/* directory. This command is wrapped in the *scripts/bender_gen_vivado_source.sh* shell script.
3. The *bender script* processes the `bender.yml` manifest and any dependent components' manifests.
4. The *bender script* generates a list of all HDL sources that make up the project.
5. Similarly, *scripts/bender_gen_prj_constraints_file_list.sh*, *scripts/bender_gen_mem_file_list.sh*, and *scripts/bender_gen_ip_file_list.sh* generate the *.xdc* constraints, *.mem* memory file list, and Vivado IP file list for the project.
6. `make` feeds these file lists into a *vivado_create_project.tcl* script.
7. The *vivado_create_project.tcl* script creates a Vivado project.
8. `make` runs the *vivado_synth.tcl* script, which opens the Vivado project and initiates synthesis. The result is a *.dcp* checkpoint file.

When you run *make hello_world_bit*, the following happens:

1. `make` first executes the *hello_world_synth* rules because *hello_world_bit* depends on *hello_world_synth*. `make` checks if FPGA (re)implementation and bitstream generation are required. If the bitstream file is up-to-date, no further action is taken.
2. `make` runs the *vivado_impl.tcl* script, which opens the Vivado project, loads the *.dcp* synthesis checkpoint, and begins the implementation phase.

When you run *make hello_world_bit_sw*, the following happens:

1. `make` runs both the *hello_world_bit* rule and the *hello_world* software build rule because *hello_world_bit_sw* depends on both.
2. If there are changes to the bitstream or the software image, the latest software is merged into the bitstream file. For details, see [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).

### Managing Vivado Logging and Tracing

The build system invokes Vivado through a wrapper script: *scripts/vivado_wrapper.sh*.  
This script defines a *VIVADO_FLAGS* variable that controls the level of Vivado logging and tracing.  

By default, the variable is set to:

```
VIVADO_FLAGS="-nolog -nojournal -notrace"
```

To debug build issues, I unset *VIVADO_FLAGS* to enable detailed logs and trace messages.

### UpdateMem and XPM Memories

Changing the contents of a *.mem* file does not require a full FPGA resynthesis and reimplementation. The memory contents are embedded within the FPGA bitstream file. Vivado's *UpdateMem* tool allows for merging new memory contents into the bitstream file post-implementation. However, *UpdateMem* works only with memories implemented using Xilinx **XPM** macros.

When Vivado implements an XPM memory, such as *xpm_memory_tdpram*, it generates a **Memory Map Information (MMI)** file, which describes how individual block RAMs compose a contiguous logical data space. The *UpdateMem* tool requires this MMI file to determine the locations in the bitstream file that need to be updated.

![Merging a .mem file into a bitstream file.](assets/merge_mem_file_into_bitstream_file.drawio.png)

*Merging a .mem file into a Bitstream File.*

*UpdateMem* also requires the instance path of the memory. In the case of BoxLambda, this path is:

```
boxlambda_soc_inst/cmem/xpm_memory_tdpram_inst/xpm_memory_base_inst
```


