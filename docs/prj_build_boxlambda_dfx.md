## The BoxLambda DFX Project Build

**BoxLambda DFX Project in the BoxLambda Directory Tree**:
    [boxlambda/gw/projects/boxlambda-dfx](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/boxlambda_dfx)

This project builds the 'official' BoxLambda DFX Configuration as described in the [Architecture section](architecture.md#the-dfx-configuration).

![DFX Terminology applied to the BoxLambda SoC](assets/dfx_terminology.png)

*DFX Terminology applied to the BoxLambda SoC.*

### The Goal: Dynamically Loading Application-Specific Gateware-Assists

The goal of adding DFX support to BoxLambda is to enable BoxLambda software applications to dynamically load an application-specific gateware component into the SoC's *Virtual Socket 0 (VS0)* placeholder component. The intent is that this component acts as a gateware-assist for the software application.

In the BoxLambda SoC, instance *boxlamba_soc_inst/vs0_inst* is set up as an RP. To demonstrate DFX, two RMs fitting this RP are created: *vs0_j1b* and *vs0_stub*. The [VS0 page](components_vs0.md) describes each in more detail.

### Building and running BoxLambda DFX

See [DFX Test](test-build-dfx.md).

### Adding a Reconfigurable Partition (RP)

The BoxLambda SoC currently just has one RP, VS0, but it is possible to create additional RPs. This section describes how to do that.

Let's call the new RP VS1.

#### Step 1: Reparameterize the DFX Controller to manage a second RP

See section [DFX Controller Parameterization](components_dfx_controller.md#dfx-controller-parameterization).

#### Step 2: Synthesize the SoC with an empty RP.

Synthesize the project, i.e. the Static Design, with an empty declaration for module *vs1*.

```
module vs1 (
  ...
);
endmodule
```

- **Directory**: *build/arty-a7-100t/gw/projects/<project_name\>*
- **Build Command**: `make <project_name>_synth`

#### Step 3: Synthesize the default RM

Synthesize the component you want to use as the default module for this RP. 

- **Directory**: *build/arty-a7-100t/gw/components/<component\>* 
- **Build Command**: `make <component_name>_synth`

Make a note of the resource utilization of the component. When we're defining the RP's Pblock, we want to make sure it's big enough to hold this RM.

#### Step 4: Mark the hierarchical instance as an RP

Using the Vivado GUI, open the SoC static synthesis checkpoint and mark the VS1 instance as an RP by assigning it the *HD.RECONFIGURABLE* property and tagging it as a black box:

```
open_project project.xpr
open_run synth_1
set_property HD.RECONFIGURABLE TRUE [get_cells boxlambda_soc_inst/vs1_inst]
update_design -quiet -cell boxlambda_soc_inst/vs1_inst -black_box
```

Keep the Vivado GUI session open. The following steps, up to [Step 6](#step-6-create-a-pblock-for-the-rp), are taken in this session.

#### Step 5: Assign the default RM to the RP

Read in the default component synthesis checkpoint and assign it to the RP *boxlambda_soc_inst/vs1_inst*. This makes the component the default RM occupying the RP.

```
read_checkpoint -cell boxlambda_soc_inst/vs0_inst vs0_stub_synth.dcp
```

#### Step 6: Create a Pblock for the RP

Create a Pblock for RP *VS1* by staking out the Pblock's region in the design's floor plan. The diagrams below shows this being done for VS0. For VS1, the procedure is the same.

![Pblock with properties](assets/pblock_w_properties.png)

*A Pblock on the Floor Plan.*

1. In the Vivado GUI, select *boxlambda_soc_inst/vs1_inst* in the Netlist window. This ensures that the Pblock we're creating is assigned to this RP.
2. In the floor plan window, click the *Draw Pblock* button.
3. Draw a rectangular region. Keep an eye on the number of selected resources (LUTs, BRAM, etc.) and make sure there are enough resources to accommodate the biggest RM variant for this RP.

    ![Pblock with resources](assets/pblock_w_resources.png)

    *When outlining a Pblock, the selected resources are shown.*

4. In the Properties view, check *RESET_AFTER_RECONFIG* and set *SNAPPING_MODE* on. *RESET_AFTER_RECONFIG* ensures that the RM is initialized after reconfiguration. *SNAPPING_MODE* aligns the Pblock region boundaries with the requirements of this 7-series FPGA.
5. Run the DFX Design Rule checks: *Reports->Report DRC*. Address any reported errors, if present.
6. Write the Pblock constraints to a file:

    ```
    write_xdc ./Sources/xdc/top_all.xdc￼
    ```

The resulting Pblock constraints look like this:

```
create_pblock pblock_vs1

add_cells_to_pblock [get_pblocks pblock_vs1] [get_cells -quiet [list boxlambda_soc_inst/vs1_inst]]

resize_pblock [get_pblocks pblock_vs1] -add {SLICE_X6Y100:SLICE_X45Y149}
resize_pblock [get_pblocks pblock_vs1] -add {DSP48_X0Y40:DSP48_X0Y59}
resize_pblock [get_pblocks pblock_vs1] -add {RAMB18_X0Y40:RAMB18_X0Y59}
resize_pblock [get_pblocks pblock_vs1] -add {RAMB36_X0Y20:RAMB36_X0Y29}

set_property RESET_AFTER_RECONFIG true [get_pblocks pblock_vs1]

set_property SNAPPING_MODE ON [get_pblocks pblock_vs1]
```

The placement of the Pblock within the floor plan significantly impacts routing. It might require some experimentation to determine which placements are routable and meet timing.

One guideline I have encountered is to first implement the design with the biggest candidate RM statically instantiated in the build (i.e. without using DFX), then check where the Vivado router placed the module (you can highlight the instance in the floor plan) and use that information to guide the placement of the RP.


Add the Pblock constraints from the previous step to the project's dfx_constraints file, specified in the project build's Bender.yml file. In case of the boxlambda_dfx project build, the constraints file is:

[gw/projects/boxlambda_dfx/constrs/pblocks.xdc](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/boxlambda_dfx/constrs/pblocks.xdc)

#### Step 8: Update the Project Build CMakeLists.txt

In the *gw_project_rules_dfx_vivado()* call of the project build's CMakeLists.txt file:

1. Add the RP instance *boxlambda_soc_inst/vs1_inst* to the VS_INSTS list.
2. Add the default RM for this RP to the VS_DEFAULT_COMPONENTS list.

The result might look like this:

```
gw_project_rules_dfx_vivado(
  TOP_MODULE boxlambda_top
  PROJECT_NAME boxlambda_dfx
  #Location of the virtual socket in the design:
  VS_INSTS
    boxlambda_soc_inst/GENERATE_VS0_MODULE.vs0_inst
    boxlambda_soc_inst/GENERATE_VS1_MODULE.vs1_inst
  VS_DEFAULT_COMPONENTS
    vs0_stub
    vs1_default
)
```

Here *vs1_default* is the default RM for VS1. See [here](components_vs0.md#creating-a-new-vs0-rm-dfx) for info on creating RMs.

#### Step 9: Route the Design and Generate the Bitstreams

- **Directory**: *build/arty-a7-100t/gw/projects/<project\>* 
- **Build Command**: `make <project_name>_bit`

This step generates multiple bitstream files:

- *<project_name\>.bin*: This is a full bitstream of the SoC, with the default RMs instantiated in the RPs.
- *<project_name\>_pblock_vs<x\>_partial.bin*: This is a partial bitstream file of just the default RM for VS<x\>. One partial bitstream file will be generated for each RP.

### Caveat: If the Static Design changes, the RMs must be re-implemented.

![pblock routing](assets/pblock_routing.png)

*Pblock selective routing constraints.*

I initially assumed that if, in addition to creating a Pblock, you also constrain the RP's Partition Pins to a specific location, then RMs and Static Design could evolve independently as long as they fit within those constraints and honor the RP interface contract.

Unfortunately, this is not the case. The problem is that, while a Pblock constrains the RM's routing to stay within the Pblock boundaries, it does *not* constrain the Static Design routing to remain outside the Pblock. As a result, part of the Static Design routing will go through the Pblock and become encoded in RM's Partial Bitstream. When the Static Design changes, its routing outside and inside the Pblock will change, requiring the RMs to be reimplemented to accommodate the updated Static Design Routing.

![static design changes in pblock](assets/static_design_changes_in_pblock.png)

For more details, see this Xilinx-AMD support article: [https://adaptivesupport.amd.com/s/article/61284?language=en_US](https://adaptivesupport.amd.com/s/article/61284?language=en_US).

