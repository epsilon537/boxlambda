The Gateware Build Structure
----------------------------
### Bender

The build system is CMake-based but relies on Bender for gateware package dependency Management:

[https://github.com/pulp-platform/bender](https://github.com/pulp-platform/bender)

Central to Bender is the package manifest *bender.yml*. In the manifest, you specify the HDL sources that make up the package, dependencies, include paths, targets (e.g. synth, sim), and associated *defines*.
A package directory is a directory containing a bender.yml file. When you run bender in that directory, you can ask it to generate a flat list of all the sources from the current package, and the packages it depends on. Optionally, it can generate that list, plus any *defines* associated with a given target, as a Tcl script. This makes integration with Vivado very easy.

![Project View of the Build System](assets/Project_Build_Diagram.png)

*Project/Component/Submodule View of the Build System.*

#### Bender Targets

Currently, the build system uses the following Bender targets:

- ***module_name***: set when building a component separately (OOC)

```
  - target: ibex_wb_core
    files:
      - rtl/ibex_wb_core_wrapper.sv    
```

- **vivado**: set when synthesizing using Vivado.
- **verilator**: set when building Verilator simulation model.
- **constraints**: set when retrieving *.xdc* constraints files for this component or project.
- **memory**: set when retrieving *.mem memory files for this component or project.

### Three Layers

The gateware system has three layers:

1. **The Project Layer (top)**: *Hello World* is an example project. A project is the top layer of the build system. The bender.yml manifest contains the top-level files of an SoC build, the project's *.xdc* constraints file, memory files used by the SoC, and a list of *components* the project depends on. 
2. **The Component Layer (middle)**: Components are the middle layer of the build system. They are the building blocks of an SoC. A component's sources, *defines*, and dependencies are defined in a bender.yml manifest. A component gets its HDL sources from its *rtl/* subdirectory and/or *sub/*, the submodule layer. I'm considering each Wishbone Bus Master or Slave a component.
3. **The Submodule Layer (bottom)**: Submodules are the bottom layer of the build system. They are the Git Submodules that BoxLambda is referencing.

### Building Gateware

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
- **_sim_sw**: Build the Verilator simulation model (*Vmodel*) of the given gateware project as well as the associated software project. The SW image memory file is copied into its proper place in the gateware build directory so it gets picked when the Verilator model executes. 
    - Depends on: *sim* target above and the software target. 
    - Build tree: *sim-a7-\** build trees. Gateware project directories only. 
- **_synth**: Synthesize the given gateware component or project. This action only exists in the *arty-a7-\** build trees. When you run the synth action on a component, Out-Of-Context (OOC) synthesis is performed. The component's input and output ports aren't hooked up to anything.
    - Depends on: gateware sources and constraints.
    - Build tree: *arty-a7-\** build trees. Gateware component and project directories.
- **_bit**: Implement the given gateware project and generate its bitstream. Note that only the gateware is built, not the SW project running on top of this gateware. The internal memory reserved for the SW program will be left empty. See the *bit_sw* action below. 
    - Depends on: *_synth* target above
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_bit_sw**: Build the give gateware project as well as the associated software project. The generated SW image memory file is merged into the FPGA bitstream file. See [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).
    - Depends on: *_bit* target above and on the software target associated with the gateware project. 
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
- **_load**: Load the gateware project's bitstream file onto the connected target.
    - Depends on: no dependencies. 
    - Build tree: *arty-a7-\** build trees. Gateware project directories only.
  
Some examples:
```
cd <boxlambda_root_dir>/build/sim-a7-35 && make ddr_test_sim_sw
```
```
cd <boxlambda_root_dir>/build/arty-a7-35 && make hello_world_synth
```
```
cd <boxlambda_root_dir>/build/arty-a7-100 && make hello_dbg_bit_sw && make hello_dbg_load
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

#### UpdateMem and XPM Memories

A change in the contents of a *.mem* file does not require full FPGA resynthesis and reimplementation. The memory contents exist somewhere in the FPGA bitstream file. With the tool **UpdateMEM**, Vivado provides a way to merge new memory contents into the bitstream file, post-implementation. There is a catch, however. *UpdateMEM* works with memories implemented using Xilinx **XPM** macros.

When Vivado implements an XPM memory such as *xpm_memory_tdpram*, a so-called **MMI** file, or **Memory Map Information** file is generated. An MMI file describes how individual block RAMs make up a contiguous logical data space. The *UpdateMem* tool needs this information to determine the locations in the bitstream file to update.

![Merging a .mem file into a bitstream file.](assets/merge_mem_file_into_bitstream_file.drawio.png)

*Merging a .mem file into a Bitstream File.*

*UpdateMem* also requires the instance path of the memory in question. In the case of BoxLambda, this path is:

```
boxlambda_soc_inst/cmem/xpm_memory_tdpram_inst/xpm_memory_base_inst
```

#### What happens when you run *make hello_world_synth*

![Gateware Build Targets.](assets/gw_build_targets.drawio.png)

*Gateware Build Targets.*

When you run *make hello_world_synth*, the following happens:

1. Make determines if (re)synthesis is needed. If synthesis is up-to-date, no further action is taken.
1. Make runs a *bender script* command on the bender.yml file in the *gw/projects/hello_world/* directory. The *bender script* command is wrapped in the *scripts/bender_gen_vivado_source.sh* shell script.
2. The bender script command processes that bender.yml manifest, as well as the bender.yml manifests of any dependent components. 
3. The bender script command emits a list of all the HDL sources that make up the project.
4. Similarly, the *scripts/bender_gen_constraints_file_list.sh* and *scripts/bender_gen_mem_file_list.sh* emit the *.xdc* constraints and .mem memory file list for the project. 
5. Make feeds these file lists into a *vivado_create_project.tcl* script. 
6. The *vivado_create_project.tcl* script creates a Vivado project.
7. Make kicks off the *vivado_synth.tcl* script which opens the Vivado project and starts synthesis. The output of synthesis is a *.dcp* checkpoint file.

When you run *make hello_world_bit*, the following happens:

1. Make will first run the *hello_world_synth* rules because *hello_world_bit* depends on *hello_world_synth*. Make determines if FPGA (re)implementation and bitstream generation are needed. If the bitstream file is up-to-date, no further action is taken. 
2. Make kicks off the *vivado_impl.tcl* script which opens the Vivado project, picks up the synthesis checkpoint *.dcp* file, and starts implementation.

When you run *make hello_world_bit_sw*, the following happens:

1. Make will run the *hello_world_bit* rule and the *hello_world* software build rule because *hello_world_bit_sw* depends on those. 
2. If there were any changes to the bitstream file or the software image, the latest software is merged into the latest bitstream file. See [UpdateMem and XPM Memories](#updatemem-and-xpm-memories).

### Verilator Lint Waivers

Rather than add lint waivers to the source code of git submodules, the waivers are grouped into *.vlt* files that live in the corresponding gateware component or project subdirectory. This way we avoid making unnecessary code changes in the git submodules.

For example 
[gw/components/ibex/lint.vlt](https://github.com/epsilon537/boxlambda/blob/master/gw/components/ibex/lint.vlt)

### Gateware Component/Project Directory Layout

A component or project directory typically contains the following files and subdirectories:

```
<component/project>
├── CMakeLists.txt
├── Bender.yml: The component/project Bender manifest.
├── lint.vlt: Lint waivers.
├── rtl
│   └── <Boxlambda specific RTL sources for given component/project>
└── sim
    └── <C++ test bench code for given component/source>
```
