Building Gateware
-----------------

Assuming all [Prerequisites](prerequisites.md) are installed, navigate to the build tree of choice and type:

```
make <gw component or project name><action>
```
where *action* is one of the following:

- **_lint**: Lint-check the given gateware component or project.
    - Depends on: gateware sources.
    - Build tree: *sim-a7-\** build trees. Gateware component and project directories.
- **_sim**: Build the Verilator simulation model (*Vmodel*) of the given gateware project. Note that only the gateware is built, not the SW project running on top of this gateware. See the *_sim_sw* target below. 
    - Depends on: gateware sources.
    - Build tree: *sim-a7-\** build trees. Gateware projects directories only.
- **_sim_sw**: Build the Verilator simulation model (*Vmodel*) of the given gateware project and the associated software project. The SW image memory file is copied into its proper place in the gateware build directory so it gets picked when the Verilator model executes. 
    - Depends on: *sim* target above and the software target. 
    - Build tree: *sim-a7-\** build trees. Gateware project directories only. 
- **_synth**: Synthesize the given gateware component or project. This action only exists in the *arty-a7-\** build trees. When you run the synth action on a component, Out-Of-Context (OOC) synthesis is performed. The component's input and output ports aren't hooked up to anything.
    - Depends on: gateware sources and constraints.
    - Build tree: *arty-a7-\** build trees. Gateware component and project directories.
- **_bit**: Implement the given gateware project and generate its bitstream. Note that only the gateware is built, not the SW project running on top of this gateware. The internal memory reserved for the SW program will be left empty. See the *bit_sw* action below. 
    - Depends on: *_synth* target above
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_bit_sw**: Build the given gateware project and the associated software project. The generated SW image memory file is merged into the FPGA bitstream file. See [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).
    - Depends on: *_bit* target above and on the software target associated with the gateware project. 
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_load**: Load the gateware project's bitstream file onto the connected target.
    - Depends on: no dependencies. 
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
  
- **_flash_gw**: Flash the gateware project's bitstream file onto the connected target.
    - Depends on: no dependencies. 
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.

Some examples:
```
cd <boxlambda_root_dir>/build/sim-a7-100 && make ddr_test_sim_sw
```
```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_world_synth
```
```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_dbg_bit_sw && make hello_dbg_load
```

The build directory tree mimics the source tree. When a build has been completed, a gateware project's Verilator model or the Vivado project files can be found under that project's directory. E.g.:
```
$ cd build/arty-a7-100/gw/projects/hello_world
$ make hello_world_synth
...
$ ls
CMakeFiles           hello_world.constraints_file_list  project.cache          project.runs    syn_util.rpt
CTestTestfile.cmake  hello_world.mem_file_list          project.dep            project.xpr
Makefile             hello_world.vivado_sources         project.hw             spram.mem
cmake_install.cmake  hello_world.vivado_sources.dep     project.ip_user_files  syn_timing.rpt
```

### What happens when you run *make hello_world_(synth|bit|bit_sw)*

![Gateware Build Targets.](assets/gw_build_targets.drawio.png)

*Gateware Build Targets.*

When you run *make hello_world_synth*, the following happens:

1. Make determines if (re)synthesis is needed. If the synthesis is up-to-date, no further action is taken.
1. Make runs a *bender script* command on the bender.yml file in the *gw/projects/hello_world/* directory. The *bender script* command is wrapped in the *scripts/bender_gen_vivado_source.sh* shell script.
2. The bender script command processes the bender.yml manifest, as well as the bender.yml manifests of any dependent components. 
3. The bender script command generates a list of all the HDL sources that make up the project.
4. Similarly, the *scripts/bender_gen_prj_constraints_file_list.sh*, *scripts/bender_gen_mem_file_list.sh*, and *scripts/bender_gen_ip_file_list.sh* emit the *.xdc* constraints, .mem memory file list, and Vivado IP file list for the project. 
5. Make feeds these file lists into a *vivado_create_project.tcl* script. 
6. The *vivado_create_project.tcl* script creates a Vivado project.
7. Make kicks off the *vivado_synth.tcl* script which opens the Vivado project and starts synthesis. The output of synthesis is a *.dcp* checkpoint file.

When you run *make hello_world_bit*, the following happens:

1. Make will first run the *hello_world_synth* rules because *hello_world_bit* depends on *hello_world_synth*. Make determines if FPGA (re)implementation and bitstream generation are needed. If the bitstream file is up-to-date, no further action is taken. 
2. Make kicks off the *vivado_impl.tcl* script which opens the Vivado project, picks up the *.dcp synthesis checkpoint, and starts implementation.

When you run *make hello_world_bit_sw*, the following happens:

1. Make will run the *hello_world_bit* rule and the *hello_world* software build rule because *hello_world_bit_sw* depends on those. 
2. If there are changes in the bitstream file or the software image, the latest software is merged into the bitstream file. See [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).

### Managing Vivado Logging and Tracing using *VIVADO_FLAGS*

The build system uses environment variable *VIVADO_FLAGS* when invoking Vivado:

```
vivado -mode batch $VIVADO_FLAGS -source ${tcl_script} -tclargs ${_tcl_args}
```

*VIVADO_FLAGS* can be used to manage the amount of Vivado logging and tracing. I normally have it set as follows:

```
export VIVADO_FLAGS=-nolog -nojournal -notrace
```

When debugging build issues, I unset *VIVADO_FLAGS* so I can check the detailed logs and trace messages.

### UpdateMem and XPM Memories

Changing the contents of a *.mem* file does not require full FPGA resynthesis and reimplementation. The memory contents exist somewhere in the FPGA bitstream file. With the tool **UpdateMEM**, Vivado provides a way to merge new memory contents into the bitstream file, post-implementation. There is a catch, however. *UpdateMEM* works with memories implemented using Xilinx **XPM** macros.

When Vivado implements an XPM memory such as *xpm_memory_tdpram*, a so-called **MMI** file, or **Memory Map Information** file is generated. An MMI file describes how individual block RAMs make up a contiguous logical data space. The *UpdateMem* tool needs this information to determine the locations in the bitstream file to update.

![Merging a .mem file into a bitstream file.](assets/merge_mem_file_into_bitstream_file.drawio.png)

*Merging a .mem file into a Bitstream File.*

*UpdateMem* also requires the instance path of the memory in question. In the case of BoxLambda, this path is:

```
boxlambda_soc_inst/cmem/xpm_memory_tdpram_inst/xpm_memory_base_inst
```

