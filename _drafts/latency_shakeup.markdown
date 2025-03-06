---
layout: post
title: 'The Latency Shakeup.'
comments: true
mathjax: yes
---

*Partial FPGA Reconfiguration* is an FPGA feature that allows you to dynamically reconfigure a portion of an FPGA design while the remaining part of the design remains unchanged and operational. The Arty A7 100T has this feature and I started adding support for it in BoxLambda. While it's still a work in progress, I have reached a point where I can dynamically load a gateware component in the BoxLambda SoC, under the control of the SoC itself. To demonstrate this feature, I developed a test program that dynamically loads a J1B CPU core and boots the SwapForth run-time firmware on this core. There are, however, some caveats.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_Ibex_Focus.png)

Summarizing the current state of affairs of BoxLambda, we have:
- An Ibex RISC-V core with machine timer and hardware interrupt support.
- Wishbone interconnect and Harvard architecture internal memory.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- A PicoRV32-based Programmable DMA Controller.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- DFX Partial FPGA Reconfiguration support.
- A *Base* and a *DFX* configuration targeting the Arty-A7-100T FPGA development board.
- A suite of test applications targeting all SoC components. Each test application runs on both FPGA configurations and on Verilator.
- Automated testing on Verilator and CocoTB.
- A Linux CMake and Bender-based Software and Gateware build system.

The BoxLambda SoC top-level: [gw/components/boxlambda_soc/rtl/boxlambda_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/boxlambda_soc/rtl/boxlambda_soc.sv)

Terminology
-----------
In the remainder of this post, I will adapt the Xilinx-AMD terminology associated with Partial FPGA Reconfiguration. Here is an overview of the terms used:
- **Dynamic Function eXchange (DFX)**: the Xilinx-AMD term for Partial FPGA Reconfiguration.
- **DFX Controller**: The DFX Controller IP provides management functions for DFX designs.  When hardware or software trigger events occur, the DFX Controller pulls partial bitstreams from memory and delivers them to an *Internal Configuration Access Port (ICAP)*.  The IP also assists with logical decoupling and startup events, which are customizable per Reconfigurable Partition.
- **ICAP**: Internal Configuration Access Port, a Vivado primitive giving access to the FPGA configuration functionality built into Xilinx-AMD FPGAs.
- **Reconfigurable Module (RM)**: An RM is the netlist or HDL description of a module that is implemented within a *Reconfigurable Partition (RP)*. Multiple RMs can exist for a given RP. For example, *vs0_stub* and *vs0_j1b* are two RM variants defined for the RP *boxlambda_soc/vs0_inst*. All RM variants belonging to an RP comply with the same interface defined by the RP.
- **Reconfigurable Partition (RP)**: An RP is a module instance in the FPGA design hierarchy marked as *Reconfigurable*. An RP is the container of an RM. In this post, the RP is instance *boxlambda_soc/vs0_inst*.
- **Virtual Socket (VS)**: a synonym for RP. Vivado documentation uses the term VS within the context of the DFX Controller IP.
- **Partition Pins**: Partition pins are the logical and physical connection between the Static Design and an RP. The Vivado toolchain automatically creates, places, and manages partition pins.
- **Pblock**: A Pblock is a user-defined region in the FPGA floor plan specifying the device resources contained within. In the context of DFX, a Pblock is associated with an RP. It defines the FPGA resources (Block RAM, LUTs,...) allocated to that RP.
- **Static Design**: The static design is the part of the design that does not change during partial reconfiguration. It includes the top-level, and all modules not defined as reconfigurable. The static design is built with static logic and static routing.

![DFX Terminology applied to the BoxLambda SoC](../assets/dfx_terminology.png)

*DFX Terminology applied to the BoxLambda SoC.*

The Goal: Dynamically Loading Application-Specific Gateware-Assists
-------------------------------------------------------------------
The goal of adding DFX support to BoxLambda is to enable BoxLambda software applications to dynamically load an application-specific gateware component into the SoC's *Virtual Socket 0 (VS0)* placeholder component. The intent is that this component acts as a gateware-assist for the software application.

At a later stage, I plan to extend DFX support in BoxLambda to allow applications to dynamically select which sound and graphics subsystem to use, including the ability to switch to application-specific sound and graphics.

The Demo: Dynamically Loading the J1B core and Booting SwapForth
----------------------------------------------------------------

J1B is a 32-bit, minimal instruction set stack processor. It's one of the supported host CPUs of the [SwapForth](https://github.com/jamesbowman/swapforth) environment.

To demonstrate DFX support in BoxLambda, I created a test program called **dfx_test** that loads a J1B core into the SoC's *Virtual Socket 0* and then boots the SwapForth run-time firmware image on this core, presenting the user with a Forth REPL.


![The DFX Test Program](../assets/dfx_test_program.png)

*The DFX Test Program.*

Section [The DFX Test Program Structure and CLI](#the-dfx-test-program-structure-and-cli) below provides more details about this test program. First, I will discuss the project breakdown.

Phases
------
I broke down the task into three *phases*, outlined below, and elaborated upon in the following sections.

**Phase 1**: Test the integration of the J1B core into BoxLambda. Create a Test SoC that statically includes the J1B core as one of its gateware components. This Test SoC does not yet have any DFX support. It runs both on FPGA and in Verilator.

**Phase 2**: Create a Test SoC that defines an RP called *VS0 (boxlambda_soc_inst/vs0_inst)*. Two RMs are created for this RP: *vs0_stub*, which is just a stub module that does *almost* nothing, and *vs0_j1b*, which is the J1B core from Phase 1 implemented as an RM. The Vivado Hardware Manager handles the bitstream loading of RMs into the RP. This test SoC does not include a DFX Controller and does not support the loading of RMs under the control of the SoC itself.

**Phase 3**: Extend the Test SoC from Phase 2 with a *DFX Controller* and create a DFX HAL and CLI that allows the dynamic loading of RMs into the RP from software. This is the *dfx_test* demo SoC.

Phase 1: Integrating the J1B Core
--------------------------------
Integrating the J1B core means fitting the J1B subsystem [as defined in the SwapForth repo](https://github.com/epsilon537/swapforth/blob/boxlambda/j1b/verilog/xilinx-top.v) to the interface I had in mind for Virtual Socket 0:

```
module vs0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
    output logic [31:0] wbm_dat_o,
    input logic [31:0] wbm_dat_i,
    output logic wbm_we_o,
    output logic [3:0] wbm_sel_o,
    output logic wbm_stb_o,
    input logic wbm_ack_i,
    input logic wbm_stall_i,
    output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [17:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);
```

The interface consists of a Wishbone master port, a Wishbone slave port, an input vector of system IRQs (the same vector seen by the Ibex CPU), and an output IRQ.

The integrated J1B core, **vs0_j1b**, looks like this:

![The J1B Core fitted to the VS0 interface](../assets/vs0_j1b.png)

*The J1B Core fitted to the VS0 interface.*

The register interface is borrowed from the PicoRV DMA Controller component. Through this interface, both Ibex and the J1B CPU have access to the J1B UART registers. The transmit direction for Ibex is the receive direction for J1B and vice versa. The Wishbone Slave port also gives the Ibex CPU access to the J1B's program memory, allowing it to install the J1B firmware. Finally, the register interface has a control register which is used to take the J1B out of reset after the firmware has been loaded into the core.

If I were to integrate the J1B core into BoxLambda properly - something I might do in the future - I would hook up the complete interface, giving the J1B core full access to the entire BoxLambda SoC. However, doing so currently would be too much of a distraction from the task at hand: demonstrating DFX in BoxLambda. Hence, for the time being, the lazy, demo-like integration shown above.

The resulting Test SoC is called **j1b_test** and looks like this:

![The j1b_test SoC](../assets/j1b_test_soc.png)

*The j1b_test Test SoC.*

On the Ibex software side, I defined a J1B Hardware Access Layer (*j1b_hal*) giving access to the core's register interface and including a function for loading the firmware into the core. The j1b_test program uses the j1b_hal to perform the following:

1. Boot the SwapForth firmware.
2. Forward the BoxLambda serial port input data to the J1B core UART input register.
3. Forward J1B core UART output data to the BoxLambda serial port.

This way, the SwapForth REPL running on the J1B core is presented to the user on the serial port terminal.

See section [Try It Yourself](#try-it-yourself) below for instructions for building and running the j1b_test on Verilator and/or the Arty A7.

**The j1b_test Test SoC project directory**:

[https://github.com/epsilon537/boxlambda/tree/master/gw/projects/j1b_test](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/j1b_test)

**The j1b_test Test Program**:

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/j1b_test/j1b_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/j1b_test/j1b_test.cpp)

Phase 2: Defining the RP and RMs
--------------------------------

![The BoxLambda SoC with RP vs0_inst and RMs vs0_stub and vs0_j1b](../assets/soc_w_rp_vs0.png)

*The BoxLambda SoC with RP VS0 (boxlambda_inst/vs0_inst) and RMs vs0_stub and vs0_j1b.*

In this step, *boxlamba_soc_inst/vs0_inst* is set up as an RP, and two RMs fitting this RP are created: *vs0_j1b* and *vs0_stub*.

*Vs0_j1b* is the J1B core from Phase 1.

*Vs0_stub* is an almost-but-not entirely empty module. All it does is properly terminate
the vs0 module interface signals, so they aren't left dangling. Wishbone reads and writes to the module are acknowledged but don't trigger any functionality.

![The vs0_stub](../assets/vs0_stub.png)

*The vs0_stub.*

(See [here](#the-core-signature-register) for some background on the signature register shown in the block diagram.)

This phase is mostly about figuring out the DFX Design Flow. The intended outcome is the following:
1. a full bitstream file representing the BoxLambda SoC with *vs0_stub* as the default module occupying RP vs0_inst.
2. a Partial Bitstream file representing RM *vs0_stub*.
3. a Partial Bitstream file representing RM *vs0_j1b*.

Step 1: Synthesize the SoC with an empty RP.
============================================
Synthesize the *dfx_test* Test project, i.e. the Static Design, with an empty declaration for module *vs0*. The steps below assume this is done in project mode with project name *project.xpr*.

```
open_project project.xpr
reset_run synth_1

# Launch Synthesis
launch_runs synth_1
wait_on_run synth_1 ; #wait for synthesis to complete before continuing

close_project
```

*vs0_inst* in the Boxlambda SoC:

```
module boxlambda_soc #(...)(...)
...

  //Virtual Socket 0 Interface
  vs0 vs0_inst (
      .sys_clk(sys_clk),
      .rst(vs0_reset),
      //32-bit pipelined Wishbone master interface 0.
      .wbm_adr_o(xbar_wbm[VS0_M].adr),
      .wbm_dat_o(xbar_wbm[VS0_M].dat_m),
      .wbm_dat_i(xbar_wbm[VS0_M].dat_s),
      .wbm_sel_o(xbar_wbm[VS0_M].sel),
      .wbm_stall_i(xbar_wbm[VS0_M].stall),
      .wbm_cyc_o(xbar_wbm[VS0_M].cyc),
      .wbm_stb_o(xbar_wbm[VS0_M].stb),
      .wbm_ack_i(xbar_wbm[VS0_M].ack),
      .wbm_we_o(xbar_wbm[VS0_M].we),
      .wbm_err_i(xbar_wbm[VS0_M].err),
      //32-bit pipelined Wishbone slave interface.
      .wbs_adr(xbar_wbs[VS0_S].adr[19:0]),
      .wbs_dat_w(xbar_wbs[VS0_S].dat_m),
      .wbs_dat_r(xbar_wbs[VS0_S].dat_s),
      .wbs_sel(xbar_wbs[VS0_S].sel),
      .wbs_stall(xbar_wbs[VS0_S].stall),
      .wbs_cyc(xbar_wbs[VS0_S].cyc),
      .wbs_stb(xbar_wbs[VS0_S].stb),
      .wbs_ack(xbar_wbs[VS0_S].ack),
      .wbs_we(xbar_wbs[VS0_S].we),
      .wbs_err(xbar_wbs[VS0_S].err),
      //Input IRQs - VS0 receives the same 32 IRQs with the same IRQ_IDs (bit
      //positions) as the Ibex CPU. The bit position assigned to the VS_0 itself is cleared, however.
      .irq_in({1'b0, fast_irqs & ~(1'b1 << IRQ_ID_VS_0), 8'b0, timer_irq, 7'b0}),
      .irq_out(fast_irqs[IRQ_ID_VS_0])
  );
...
endmodule
```

Empty vs0 module declaration (In practice, I'm using the [vs0_stub source](https://github.com/epsilon537/boxlambda/blob/master/gw/components/vs0_stub/rtl/vs0.sv) with the module body *ifdef'd* out):

```
module vs0 (
    input logic sys_clk,
    input logic rst,

    //32-bit pipelined Wishbone master interface.
    output logic [27:0] wbm_adr_o,
    output logic [31:0] wbm_dat_o,
    input logic [31:0] wbm_dat_i,
    output logic wbm_we_o,
    output logic [3:0] wbm_sel_o,
    output logic wbm_stb_o,
    input logic wbm_ack_i,
    input logic wbm_stall_i,
    output logic wbm_cyc_o,
    input logic wbm_err_i,

    //32-bit pipelined Wishbone slave interface.
    input logic [17:0] wbs_adr,
    input logic [31:0] wbs_dat_w,
    output logic [31:0] wbs_dat_r,
    input logic [3:0] wbs_sel,
    output logic wbs_stall,
    input logic wbs_cyc,
    input logic wbs_stb,
    output logic wbs_ack,
    input logic wbs_we,
    output logic wbs_err,

    //Input IRQs from the rest of the system.
    input wire [31:0] irq_in,
    //Output IRQ signal
    output wire irq_out
);
endmodule
```


Step 2: Synthesize the RMs
==========================
Synthesize components *vs0_stub* and *vs0_j1b* Out of Context (OOC). For each, generate a synthesis checkpoint: *vs0_stub_synth.dcp* and *vs0_j1b_synth.dcp*.

Make a note of the resource utilization of *vs0_j1b*. When we're defining the RP's Pblock, we want to make sure it's big enough to hold this RM.

Step 3: Mark the hierarchical instance as an RP
===============================================
Using the Vivado GUI, open the SoC static synthesis checkpoint and mark *boxlambda_soc_inst/vs0_inst* as an RP by assigning it the *HD.RECONFIGURABLE* property and tagging it as a black box:

```
open_project project.xpr
open_run synth_1
set_property HD.RECONFIGURABLE TRUE [get_cells boxlambda_soc_inst/vs0_inst]
update_design -quiet -cell boxlambda_soc_inst/vs0_inst -black_box
```

Keep the Vivado GUI session open. The following steps, up to [Step 7](#step-7-create-the-static-route-checkpoint), are taken in this session.

Step 4: Assign the default RM to the RP
=======================================
Read in the *vs0_stub* synthesis checkpoint and assign it to RP *boxlambda_soc_inst/vs0_inst*. This makes *vs0_stub* the default RM occupying the RP.

```
read_checkpoint -cell boxlambda_soc_inst/vs0_inst vs0_stub_synth.dcp
```

Step 5: Create a Pblock for the RP
==================================
Create a Pblock for RP *VS0* by staking out the Pblock's region in the design's floor plan.

![Pblock with properties](../assets/pblock_w_properties.png)

*A Pblock on the Floor Plan.*

1. In the Vivado GUI, select *boxlambda_soc_inst/vs0_inst* in the Netlist window. This ensures that the Pblock we're creating is assigned to this RP.
2. In the floor plan window, click the *Draw Pblock* button.
3. Draw a rectangular region. Keep an eye on the number of selected resources (LUTs, BRAM, etc.) and make sure there are enough resources to accommodate the biggest RM variant for this RP.

    ![Pblock with resources](../assets/pblock_w_resources.png)

    *When outlining a Pblock, the selected resources are shown.*

4. In the Properties view, check *RESET_AFTER_RECONFIG* and set *SNAPPING_MODE* on. *RESET_AFTER_RECONFIG* ensures that the RM is initialized after reconfiguration. *SNAPPING_MODE* aligns the Pblock region boundaries with the requirements of this 7-series FPGA.
5. Run the DFX Design Rule checks: *Reports->Report DRC*. Address any reported errors, if present.
6. Write the Pblock constraints to a file:

    ```
    write_xdc ./Sources/xdc/top_all.xdc￼
    ```

The resulting Pblock constraints look like this:

```
#DFX (Partial FPGA Reconfiguration) constraints file for Virtual Socket 0 (VS0).
#These constraints define the location and dimensions of VS0's partition block on the FPGA's
#floorplan.

#Partition block name:
create_pblock pblock_vs0

#The module instance to associate this partition block with:
add_cells_to_pblock [get_pblocks pblock_vs0] [get_cells -quiet [list boxlambda_soc_inst/vs0_inst]]

#Region location:
resize_pblock [get_pblocks pblock_vs0] -add {SLICE_X6Y100:SLICE_X45Y149}
resize_pblock [get_pblocks pblock_vs0] -add {DSP48_X0Y40:DSP48_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB18_X0Y40:RAMB18_X0Y59}
resize_pblock [get_pblocks pblock_vs0] -add {RAMB36_X0Y20:RAMB36_X0Y29}

#Indicate that the DFX controller must perform a reset of VS0 after reconfiguration.
set_property RESET_AFTER_RECONFIG true [get_pblocks pblock_vs0]

#Indicate that the partition block boundaries of VS0 given above must snap to the proper grid boundaries
#for 7 series FPGAs.
set_property SNAPPING_MODE ON [get_pblocks pblock_vs0]
```

The placement of the Pblock within the floor plan significantly impacts routing. It might require some experimentation to determine which placements are routable and meet timing. For some projects, this could be a challenging task, but for Boxlambda, with its relatively slow clock and plenty of free space on the device, it was not an issue.

One guideline I have encountered is to first implement the design with the biggest candidate RM statically instantiated in the build (i.e. without using DFX), then check where the Vivado router placed the module (you can highlight the instance in the floor plan) and use that information to guide the placement of the RP.

Step 6: Route the Design and Generate the Bitstreams
====================================================
```
opt_design
place_design
route_design
write_bitstream -force -bin_file dfx_project
```

This *write_bitstream* command generates two bitstream files:
- *dfx_project.bin*: This is a full bitstream of the SoC, with *vs0_stub* instantiated in the RP.
- *dfx_project_pblock_vs0_partial.bin*: This is a partial bitstream file of just the *vs0_stub* RM.

Step 7: Create the Static Route Checkpoint
==========================================
Before writing the static route checkpoint, the *vs0_stub* RM has to be cleared from the RP and the routing has to be locked down:

```
update_design -cell boxlambda_soc_inst/vs0_inst -black_box
lock_design -level routing
write_checkpoint -force dfx_project.static_route.dcp
```

At this point, the Vivado session may be closed.

Step 8: Create an implementation project for the Alternate RM
=============================================================
The implementation project consists of the routed static design checkpoint and the synthesized RM checkpoint:
```
create_project -force -part xc7a100ticsg324-1L rm_impl_project
add_files dfx_project.static_route.dcp
add_files vs0_j1b_synth.dcp
```

Step 9: Assign the alternate RM to the RP and Link
==================================================
```
set_property SCOPED_TO_CELLS boxlambda_soc_inst/vs0_inst [get_files vs0_j1b_synth.dcp]
#Link the reconfigurable module to the static design.
link_design -mode default -reconfig_partitions boxlambda_soc_inst/vs0_inst -part xc7a100ticsg324-1L
```

Step 10: Route the Alternate Design and Generate the Bitstreams
===============================================================
```
opt_design
place_design
route_design
write_bitstream -force -bin_file vs0_j1b
```

This *write_bitstream* command generates two bitstream files:
- *vs0_j1b.bin*: This is a full bitstream of the SoC, with vs0_j1b instantiated in the RP.
- *vs0_j1b_pblock_vs0_partial.bin*: This is a partial bitstream file of the *vs0_j1b* RM.

Testing
=======
1. Using the Vivado Hardware Manager, load the *dfx_project.bin* full bitstream onto the target. The target is now running the BoxLambda SoC with the *vs0_stub* RM occupying the *VS0* RP.
2. Using the Vivado Hardware Manager, load the *vs0_j1b_pblock_vs0_partial.bin* partial bitstream onto the target. The *vs0_stub* RM is now replaced with the *vs0_j1b* RM.
3. Flash the *j1b_test* software image onto the target. The test should run exactly as on the static *j1b_test* SoC from Phase 1.

Scripts and Build Rules
=======================
I captured the above steps into a handful of Vivado scripts executed by BoxLambda's build system:
- **Step 1**: Project Synthesis:
    - **Directory**: *build/arty-a7-100t/gw/projects/\<project>*
    - **Build Command**: `make <project_name>_synth`
    - **Vivado Scripts Invoked**: [scripts/vivado_create_project.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_create_project.tcl) followed by [scripts/vivado_synth.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_synth.tcl).
- **Step 2**: Component OOC Synthesis:
    - **Directory**: *build/arty-a7-100t/gw/components/\<component>*
    - **Build Command**: `make <component_name>_synth`
    - **Vivado Scripts Invoked**: [scripts/vivado_create_project.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_create_project.tcl) followed by [scripts/vivado_synth.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_synth.tcl) (same scripts as used for project synthesis, but the *create_project* script is invoked with a *-ooc* flag).
- **Steps 3-7**: Building the project's static bitstream file as well as the bitstream file of the default RM:
    - **Directory**: *build/arty-a7-100t/gw/projects/\<project>*
    - **Build Command**: `make <project_name>_bit`
    - **Vivado Script Invoked**: [scripts/vivado_impl_dfx_prj.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_impl_dfx_prj.tcl)
- **Steps 8-10**: Building the alternate RM's bitstream file:
    - **Directory**: *build/arty-a7-100t/gw/components/\<component>*
    - **Build Command**: `make <component_name>_bit`
    - **Vivado Script Invoked**: [scripts/vivado_impl_dfx_rm.tcl](https://github.com/epsilon537/boxlambda/blob/master/scripts/vivado_impl_dfx_rm.tcl).

The [Try It Yourself: DFX Test](#the-dfx-test-on-fpga) section below walks you through the complete build and test procedure.

Phase 3: Adding the DFX Controller
----------------------------------

DFX Controller Operation
========================

To manage the loading of an RM into an RP from the SoC itself, a DFX Controller needs to be added to the design. The DFX Controller is a Vivado IP providing management functions for DFX designs across many use cases. The hardware-triggered DFX use cases involve complex state machines and require a lot of configuration. However, for BoxLambda, the use case is pretty simple: loading an RM into an RP under software control. Here's how it works:

1. A software program loads the RM's partial bitstream file into a memory buffer.

    ![DFX Flow 1](../assets/dfx_flow_1.png)

2. The software program shuts down the DFX Controller, configures the address and size of the bitstream memory buffer in the DFX Controller's *BS INFO* registers, then re-enables the DFX Controller.

    ![DFX Flow 2](../assets/dfx_flow_2.png)

3. The software program issues a trigger to the DFX Controller using the *SW_TRIGGER* register. This causes the DFX Controller to start fetching the bitstream data from the configured memory location and feeding it to the internal configuration access port (ICAP).

    ![DFX Flow 3](../assets/dfx_flow_3.png)

4. When the RP is configured, the DFX Controller resets the RP/RM.

    ![DFX Flow 4](../assets/dfx_flow_4.png)

The software program monitors the progression via the DFX Controller status register.

The *dfx_test* program contains a software routine that implements the complete sequence. See function *dfx_load_module()* in file [sw/projects/dfx_test/dfx_cli.cpp](https://github.com/epsilon537/boxlambda/blob/98b4f4e6d4bf33efa24f122d65c103f06b76289e/sw/projects/dfx_test/dfx_cli.cpp).

Binary for ICAP
===============
The RM's partial bitstream, which is read from memory by the DFX Controller, is not in the regular bitstream file format generated by the *write_bitstream* command. The DFX Controller IP comes with a Tcl API function to convert the partial bitstream file into the format accepted by the ICAP primitive. The Tcl script is invoked as follows:

```
#Convert partial.bin file into bin_for_icap format
source [get_property REPOSITORY [get_ipdefs *dfx_controller:1.0]]/xilinx/dfx_controller_v1_0/tcl/api.tcl
dfx_controller_v1_0::format_bin_for_icap -bs 1 -i vs0_j1b_vs0_partial.bin
```

The result of calling this script is a *.bin_for_icap* file.

I added this Tcl snippet to the *vivado_impl_dfx_rm.tcl* script mentioned in the [Scripts](#scripts) section above.

DFX Controller Parameterization
===============================

![DFX Controller Options](../assets/dfx_controller_options.png)

*DFX Controller Selected Parameters.*

The DFX Controller requires a bit of parameterization, as shown in the screenshots above.
- The AXI Lite interface is enabled, allowing software to access the DFX Controller registers.
- The controller is managing a 7 series device.
- ICAP arbitration is not required. The DFX Controller is the only ICAP client in this design.
- FIFO depth and clock domain stages are set to the lowest possible values. Everything is running in one single clock domain so the clock domain crossing stages are not needed, but there is no option to remove them altogether.
- Active High reset, one clock cycle wide.
- Bitstream address and size are irrelevant. They get reconfigured at run-time by software.
- Just one Virtual Socket Manager, to manage one RP.
- The number of RMs and triggers is set to two. Initially, I assumed that switching from one RM to another required switching between two table entries in the DFX Controller. However, later I learned that this is not the case. One RM entry is sufficient. You only need to reprogram that single entry and issue a trigger when ready. So even though my DFX Controller instance currently has two RM entries allocated, I am only using entry 0.

The resulting DFX Controller instance and register map look like this:
![DFX Controller IP and Register Map](../assets/dfx_controller_ip_addr_map.png)

*DFX Controller IP and Register Map.*

Wishbone to AXI Interworking
============================
The DFX Controller IP bus interfaces are AXI-based. BoxLambda is Wishbone based. To bridge the two bus protocols, I'm using ZipCPU's [wbm2axilite](https://github.com/epsilon537/wb2axip/blob/c8dd694b472e74c53dcf9fa588b64e2b10ef65c0/rtl/wbm2axilite.v) and [aximrd2wbsp](https://github.com/epsilon537/wb2axip/blob/c8dd694b472e74c53dcf9fa588b64e2b10ef65c0/rtl/aximrd2wbsp.v). The resulting **wb_dfx_controller** module looks like this:


![WB DFX Controller Block Diagram](../assets/wb_dfx_controller.png)

*Wb_dfx_controller block diagram.*

Here's the RTL:

[gw/components/wb_dx_controller/rtl/wb_dfx_controller.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/wb_dfx_controller/rtl/wb_dfx_controller.sv)

The DFX Test Program Structure and CLI
--------------------------------------

![dfx test sw](../assets/dfx_test_sw.png)

*Structure of the dfx_test software program.*

The DFX Test Program is not an automatic test case like the previous BoxLambda test cases. It runs exclusively on the Arty-A7-100T and requires user interaction through a CLI. The CLI commands are grouped into modules:

- [dfx_cli](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/dfx_test/dfx_cli.cpp): This is a CLI wrapper around the [dfx_hal](https://github.com/epsilon537/boxlambda/blob/master/sw/components/dfx/dfx_controller_hal.h) component. Most commands let you interact with the DFX Controller at a low level. However, there's one high-level command, **dfx_load_module**, that implements the entire sequence of loading an RM's bitstream file from the filesystem into the VS0 RP.

    ```
    * dfx_control
            dfx_control <cmd> <extra byte> <extra halfword> : Write to DFX Control Register.
     * dfx_status
            Retrieve DFX status.
     * dfx_trig_get
            Read DFX Trigger registers.
     * dfx_trig_set
            dfx_trig_set <trig_id>
     * dfx_rm_info_get
            Get Reconfigurable Module Info
     * dfx_bs_info_get
            Get Bitstream Info
     * dfx_bs_info_set
            dfx_bs_info_set <idx> <hex address> <size in bytes>
     * dfx_read_core_sig
            Read core's signature register
     * dfx_load_module
            dfx_load_module <filename>
    ```
- [j1b_cli](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/dfx_test/j1b_cli.cpp): This CLI allows you to boot the SwapForth firmware image on the J1B core and to transfer serial port I/O to the J1B providing access to its REPL.

    ```
     * j1b_boot
            j1b_boot <filename>: Boot J1B core with given FW image.
     * j1b_fwd_uart
            Forward UART I/O to J1B.
    ```

- [mem_fs_cli](https://github.com/epsilon537/boxlambda/blob/master/sw/components/mem_fs_cli/mem_fs_cli.cpp): This CLI module provides file system commands such as *ls* and *rm* as well as commands for loading files into memory and save memory buffers to files.

    ```
     * rm
            rm <filename> : Remove (delete) file.
     * save
            save <filename> <address> <size in bytes> : write memory contents to file.
     * load
            load <filename> [address] : read file into memory. Alloc mem. buf. if addr. not given.
     * allocBuf
            allocBuf <size> : allocate from heap buffer of given size.
     * relBuf
            relBuf <address> : release the buffer at given adress.
     * ls
            list directory contents.
    ```

- [ymodem_cli](https://github.com/epsilon537/boxlambda/blob/master/sw/components/ymodem_cli/ymodem_cli.cpp): The **ymodem_rx** command allows you to transfer files from the host PC to BoxLambda's SD Card file system. I use it to transfer the RM bitstreams and J1B firmware to BoxLambda. While I could copy everything onto an SD card and then move that card from PC to BoxLambda, I prefer this method as it involves fewer moving parts.

    ```
     * ymodem_rx
            ymodem_rx <filename>: Ymodem rx and save to give file.
     * ymodem_tx_buf
            ymodem_tx_buf <filename> <hex address> <size_in_bytes>: Ymodem transmit given memory buffer with given filename
    ```

The Core Signature Register
===========================

The block diagram above shows a [vs0_hal](https://github.com/epsilon537/boxlambda/blob/master/sw/components/vs0_hal/vs0_hal.h). This HAL contains definitions that are common across all VS0 RM variants (currently just *vs0_j1b* and *vs0_stub*):
- The base address and size of the VS0 RM address space.
- The VS0 Signature Register at the end of the VS0 RM address space. All VS0 RM variants are expected to implement this register and return a value that is unique to the RM. This allows software to identify the RM loaded into the RP before attempting to access RM-specific registers or memory. The *dfx_read_core_sig* command in the *dfx_cli* module reads this register.

Caveats
-------

If the Static Design changes, the RMs must be re-implemented.
=============================================================
![pblock routing](../assets/pblock_routing.png)

*Pblock selective routing constraints.*

I initially assumed that if, in addition to creating a Pblock, you also constrain the RP's Partition Pins to a specific location, then RMs and Static Design could evolve independently as long as they fit within those constraints and honor the RP interface contract.

Unfortunately, this is not the case. The problem is that, while a Pblock constrains the RM's routing to stay within the Pblock boundaries, it does *not* constrain the Static Design routing to remain outside the Pblock. As a result, part of the Static Design routing will go through the Pblock and become encoded in RM's Partial Bitstream. When the Static Design changes, its routing outside and inside the Pblock will change, requiring the RMs to be reimplemented to accommodate the updated Static Design Routing.

![static design changes in pblock](../assets/static_design_changes_in_pblock.png)

It might be possible to work around this limitation by mapping the static logic into a separate Pblock. I'm looking into that.

For more details, see this Xilinx-AMD support article: [https://adaptivesupport.amd.com/s/article/61284?language=en_US](https://adaptivesupport.amd.com/s/article/61284?language=en_US).

DFX is Xilinx-AMD Specific
==========================
I can't accurately verilate BoxLambda SoCs containing a DFX Controller and I can't directly port them to FPGA devices from other vendors.

I can still verilate BoxLambda SoCs with a specific RM statically included in the build, however. That is effectively what *j1b_test* is. It's just the process of loading an RM into an RP that I can't verilate.

Btw, *dfx_test* can be built in the *sim-a7-100* Verilator build tree. In this build configuration, *dfx_test* is a BoxLambda SoC without a DFX Controller and with a *vs0_stub* statically included. The test case simply lets the software boot on the SoC and checks that the *vs0_stub's* signature register can be read.

Regarding the lack of portability, I will treat DFX as an optional feature. Similar to other BoxLambda components like USB, I2C, and SPI Flash, DFX can be included or excluded from the SoC via compile flags. If BoxLambda is ported to another FPGA platform, features can be enabled or disabled to fit that platform.

Other Changes
-------------
Not all changes I make between blog posts are directly related to the topic at hand. In the past, I used to capture these updates here, but moving forward, I’ll track them in a dedicated [CHANGELOG.md](https://github.com/epsilon537/boxlambda/blob/master/CHANGELOG.md) file.

Try It Yourself
---------------

Setup
=====
1. Install the [Software Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
2. Get the BoxLambda repository:

    ```
    git clone https://github.com/epsilon537/boxlambda/
    cd boxlambda
    ```

3. Switch to the **dfx** tag:

    ```
    git checkout dfx
    ```

4. Set up the repository. This initializes the git submodules used and creates the default build trees:

    ```
    ./boxlambda_setup.sh
    ```

The J1B Test on Verilator
=========================

1. Build the **j1b_test** project in the *sim-a7-100* build tree:

    ```
    cd build/sim-a7-100/gw/projects/j1b_test
    make j1b_test_sim_sw
    ```

2. Execute the generated Verilator model. You should see the following output:

    ```
    ./Vmodel
    DUT: Starting...
    DUT: Reading J1B core signature register...
    DUT: Signature correct.
    DUT: J1B program length in bytes: 32768
    DUT: Taking J1B out of reset...
    DUT: Sending test string: 42 EMIT
    DUT:
    DUT: Test string sent. Forwarding UART...
    DUT:  * ok
    SIM: String matched.
    SIM: Test passed.
    ```

The test string *42 emit* is a Forth instruction, requesting to *emit* (print) the character with ASCII code 42 (an asterisk). *\* ok* is the response from the J1B processor.

The J1B Test on FPGA
====================

1. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.

2. Build the project in *arty-a7-100t* build tree:

    ```
    cd build/arty-a7-100/gw/projects/j1b_test
    make j1b_test_bit_sw
    ```

4. Download the generated bitstream file to the Arty A7:

    ```
    make j1b_test_load
    ```

5. You should see the same output as in the Verilator test above.
6. The REPL is still listening. You can continue entering Forth instructions:

    ```
    : star 42 emit ;  ok
    cr star cr star cr star
    *
    *
    * ok
    ```

The DFX Test on FPGA
====================

1. Hook up the MicroSD PMOD as described [here](https://boxlambda.readthedocs.io/en/latest/pmods/#microsd-pmod) and insert a FAT formatted SD card.

2. Connect a terminal program to Arty's USB serial port. I suggest using a terminal program that supports Ymodem transfers such as *Minicom*. **Settings: 115200 8N1**.

3. Build the *dfx_test* software project in the arty-a7-100 build tree:

    ```
    cd build/arty-a7-100/sw/projects/dfx_test
    make dfx_test
    ```

4. Flash the *dfx_test* program onto the target:

    ```
    make dfx_test_flash_sw
    ```

5. Build the *dfx_test* gateware project in the *arty-a7-100* build tree:

    ```
    cd build/arty-a7-100/gw/projects/dfx_test
    make dfx_test_bit
    ```

6. Flash the gateware build onto the target:

    ```
    make dfx_test_flash_gw
    ```

7. When flashing has been completed, the target should boot up. You should see the following messages:

    ```
    Starting...
    Initializing SDRAM @0x20000000...
    Switching SDRAM to software control.
    Read leveling:
      m0, b00: |00000000000000000000000000000000| delays: -
      m0, b01: |11111111111111111111111111111100| delays: 14+-14
      m0, b02: |00000000000000000000000000000000| delays: -
      m0, b03: |00000000000000000000000000000000| delays: -
      m0, b04: |00000000000000000000000000000000| delays: -
      m0, b05: |00000000000000000000000000000000| delays: -
      m0, b06: |00000000000000000000000000000000| delays: -
      m0, b07: |00000000000000000000000000000000| delays: -
      best: m0, b01 delays: 14+-14
      m1, b00: |00000000000000000000000000000000| delays: -
      m1, b01: |11111111111111111111111111111100| delays: 14+-14
      m1, b02: |00000000000000000000000000000000| delays: -
      m1, b03: |00000000000000000000000000000000| delays: -
      m1, b04: |00000000000000000000000000000000| delays: -
      m1, b05: |00000000000000000000000000000000| delays: -
      m1, b06: |00000000000000000000000000000000| delays: -
      m1, b07: |00000000000000000000000000000000| delays: -
      best: m1, b01 delays: 14+-14
    Switching SDRAM to hardware control.
    SDRAM init OK.
    Mounting filesystem...
    CID: 534d5402:47323341:7d604971:3168018d
    Reading VS0 core signature register...
    Read signature value: 0x510b
    Starting CLI...
    ```

8. Build the *vs0_j1b* RM component in the *arty-a7-100* build tree:

    ```
    cd build/arty-a7-100/gw/components/vs0_j1b
    make vs0_j1b_bit
    ```

9. Build the *vs0_stub* RM component in the *arty-a7-100* build tree:

    ```
    cd build/arty-a7-100/gw/components/vs0_stub
    make vs0_stub_bit
    ```

10. Transfer the following files to an SD Card and rename them to something short:

    - **build/arty-a7-100/gw/components/vs0_j1b/vs0_j1b_pblock_vs0_partial.bin.bin_for_icap**: This is the *vs0_j1b* RM Partial Bitstream file. Let's rename it to *vs0_j1b*.
    - **build/arty-a7-100/gw/components/vs0_stub/vs0_stub_pblock_vs0_partial.bin.bin_for_icap**: This is the *vs0_stub* RM Partial Bitstream file. Let's rename it to *vs0_stub*.
    - **sw/projects/dfx_test/nuc.bin**: This is the SwapForth firmware image for the J1B processor.

11. Insert the SD Card in BoxLambda's MicroSD card slot.

    As an aside, instead of copying the files to an SD Card mounted on your PC and then moving the SD Card to BoxLambda, you can leave a card inserted into BoxLambda's SD Card slot and transfer the files using the Ymodem protocol. To do that, enter the following command on the CLI:

    ```
    ymodem_rx <filename>
    ```

    Then send the file in question using your terminal program's ymodem function. The transferred file will be saved on the SD card as *\<filename\>*.

12. Confirm that all the required files are on the filesystem by running the *ls* CLI command:

    ```
    > ls
       <DIR>   SYSTEM~1
         32768 NUC.BIN
        592780 VS0_J1B
        592780 VS0_STUB
    1 dirs, 3 files.
    ```

13. At this point, the RM equipped in the VS0 RP is *vs0_stub*. Let's switch over to the *vs0_j1b* RM. Enter the following command on the CLI:

    ```
    > dfx_load_module vs0_j1b
    Loading file tst_j1b, size: 592780 bytes, into memory at address 0x20000368.
    Issuing DFX trigger...
    DFX loading...
    DFX state: 4
    DFX state: 7
    Done.
    ```

14. Now we can boot the J1B firmware:

    ```
    > j1b_boot nuc.bin
    Reading core signature register...
    Read signature value: 0xf041011b
    Signature correct.
    Loading file nuc.bin, size: 32768 bytes, into memory at address 0x20000368.
    Booting J1B...
    Taking J1B out of reset...
    Done.
    ```

15. Finally, we forward the serial port to the J1B core so we can access its REPL:

    ```
    > j1b_fwd_uart
    Reading core signature register...
    Read signature value: 0xf041011b
    Signature correct. Checking if J1B has been booted up...
    Forwarding UART. Press <ESC> to return.
    ```

16. You can now start entering Forth instructions.

    ```
    CR 42 emit CR
    *
     ok
    ```

17. Optionally, exit out of the REPL and switch back to the *vs0_stub* RM:

    ```
    Returning to shell...
    > dfx_load_module vs0_stub
    Loading file tst_stub, size: 592780 bytes, into memory at address 0x20008370.
    Issuing DFX trigger...
    DFX loading...
    DFX state: 4
    DFX state: 7
    Done.
    >
    ```

Conclusion
----------
DFX support in BoxLambda is a work in progress. To complete the feature, I need to address the following topics:
- Add support for multiple RPs, specifically, one for the graphics subsystem and one for the sound subsystem.
- Constrain the static logic to its own Pblock. The intent is to allow independent modification and implementation of the RMs and the BoxLambda *proper* (the static logic) while respecting the Pblock boundaries and their interfaces.

Is the second bullet item feasible? I'll let you know in the next post.

References
----------
- The ICAP Primitive: [ICAPE3](https://docs.amd.com/r/en-US/ug974-vivado-ultrascale-libraries/ICAPE3)
- The DFX Controller: [PG374](https://docs.amd.com/v/u/en-US/pg374-dfx-controller)
- DFX User Guide: [UG909](https://docs.amd.com/r/en-US/ug909-vivado-partial-reconfiguration/Introduction-to-Dynamic-Function-eXchange).
- DFX Tutorial: [UG947](https://docs.amd.com/r/en-US/ug947-vivado-partial-reconfiguration-tutorial/Introduction).
- SwapForth: [https://github.com/jamesbowman/swapforth](https://github.com/jamesbowman/swapforth).
- Wb2axip: [https://github.com/ZipCPU/wb2axip](https://github.com/ZipCPU/wb2axip).


