---
hide:
  - toc
---

# The Gateware Build Structure

## Bender

The build system is CMake-based but relies on Bender for gateware package dependency management:

[https://github.com/pulp-platform/bender](https://github.com/pulp-platform/bender)

Central to Bender is the package manifest file, `bender.yml`. This manifest specifies HDL sources, Vivado IPs, dependencies, include paths, targets (e.g., synthesis, simulation), and associated *defines*.

A **package directory** is a directory containing a `bender.yml` file. Running Bender in such a directory generates a flat list of all sources from the current package and its dependencies. Additionally, Bender can output this list along with any *defines* for a given target as a Tcl script, simplifying Vivado integration.

![Project View of the Build System](assets/Project_Build_Diagram.png)

*Project/Component/Submodule View of the Build System.*

### Bender Targets

The build system currently uses the following Bender targets:

- ***module_name***: Used when building a component separately (out-of-context).

    ```yaml
    - target: ibex_wb_core
      files:
        - rtl/ibex_wb_core_wrapper.sv
    ```

- **vivado**: Set when synthesizing using Vivado.
- **verilator**: Set when building a Verilator simulation model.
- **prj_constraints**: Set when retrieving `.xdc` constraint files for the project.
- **dfx_constraints**: For DFX-enabled builds, set when retrieving *Pblock* constraint files for the project.
- **memory_vivado**: Set when retrieving `.mem` files required for Vivado synthesis of the component or project.
- **memory_verilator**: Set when retrieving `.mem` files needed for Verilator simulations.
- **vivado_ip_arty-a7-100**: Set in Arty-A7-100 builds when retrieving `.xci` files of Vivado IPs referenced in the component or project.
- **vivado_ip_arty-a7-35**: Set in Arty-A7-35 builds for `.xci` files of Vivado IPs. **(Deprecated)**

## Three Layers

The gateware system is organized into three layers:

1. **The Project Layer (Top)**:
   Example: *Hello World*. This layer represents the top level of the build system. The `bender.yml` manifest includes top-level SoC build files, project-specific `.xdc` constraints, memory files, and the list of components the project depends on.

2. **The Component Layer (Middle)**:
   Components form the building blocks of an SoC. Each component defines its sources, *defines*, and dependencies in a `bender.yml` manifest. Component HDL sources are located in the `rtl/` subdirectory or in `sub/` (submodule layer). Each Wishbone Bus master or slave is considered a separate component.

3. **The Submodule Layer (Bottom)**:
   Submodules are Git submodules referenced by BoxLambda. They form the foundation of the build system.

## Verilator Lint Waivers

Instead of adding lint waivers directly to the source code of Git submodules, these waivers are grouped into `.vlt` files located in the corresponding gateware component or project subdirectory. This approach avoids unnecessary changes to submodule source code.

Example:
[gw/components/ibex/lint.vlt](https://github.com/epsilon537/boxlambda/blob/master/gw/components/ibex/lint.vlt)

## Gateware Component/Project Directory Layout

A typical component or project directory includes the following structure:

```
<component/project>
├── CMakeLists.txt
├── Bender.yml: The component/project Bender manifest.
├── lint.vlt: Lint waivers.
├── rtl/
│   └── <BoxLambda-specific RTL sources for the component/project>
└── sim/
    └── <C++ test bench code for the component/project>
```
