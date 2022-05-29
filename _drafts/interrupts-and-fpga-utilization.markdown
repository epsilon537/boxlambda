---
layout: post
title: 'Interrupts, and estimated FPGA Resource Utilization.'
comments: true
---

About interrupts
----------------

Our CPU supports the following interrupts (taken from [https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html)):

**Ibex Interrupts:**

| Interrupt Input Signal  | ID    | Description                                      |
|-------------------------|-------|--------------------------------------------------|
| ``irq_nm_i``            | 31    | Non-maskable interrupt (NMI)                     |
| ``irq_fast_i[14:0]``    | 30:16 | 15 fast, local interrupts                        |
| ``irq_external_i``      | 11    | Connected to platform-level interrupt controller |
| ``irq_timer_i``         | 7     | Connected to timer module                        |
| ``irq_software_i``      | 3     | Connected to memory-mapped (inter-processor)     |
|                         |       | interrupt register                               |

### The Timer

The RISC-V spec includes a timer specification: RISC-V Machine Timer Registers (see RISC-V Privileged Specification, version 1.11, Section 3.1.10). The Ibex GitHub repository contains a compliant implementation as part of the *Simple System* example:

[https://github.com/epsilon537/ibex/tree/master/examples/simple_system](https://github.com/epsilon537/ibex/tree/master/examples/simple_system)

We'll be using this timer module implementation, so we don't need a separate PIT module.

The Timer module flags interrupts via signal *irq_timer_i*. The CPU sees this as IRQ ID 7.

### The Fast Local Interrupts

We can freely assign 15 local interrupts. I've got the following list:

- 1 interrupt line per Reconfigurable Module (RM), so 3 in total. The default RMs are VERA and a Dual JT49. VERA uses one interrupt line, JT49 uses none.
- 1 interrupt line each for:
  - wbuart
  - sdspi
  - wbi2c
  - ps2_mouse
  - ps2_keyboard
  - Praxos DMA
  - Quad SPI
  - ICAP
  - DFX Controller
  - GPIO. 
  
  That's 10 interrupts in total.

The interrupts are serviced in order of priority, the highest number being the highest priority.

I have ordered the Fast Local interrupts as follows:

**Fast Local Interrupt Assignments:**

| Interrupt Input Signal  | ID    | Description                             |
|=========================|=======|=========================================|
| ``irq_fast_i[14]``      | 30    | RM_2 interrupt (Default: not assigned)  |
| ``irq_fast_i[13]``      | 29    | RM_1 interrupt (Default: VERA IRQ)      |
| ``irq_fast_i[12]``      | 28    | RM_0 interrupt (Default: not assigned)  |
| ``irq_fast_i[11]``      | 27    | Praxos DMAC IRQ                         |
| ``irq_fast_i[10]``      | 26    | sdspi IRQ                               |
| ``irq_fast_i[9]``       | 25    | wbuart IRQ                              |
| ``irq_fast_i[8]``       | 24    | ps2_keyboard IRQ                        |
| ``irq_fast_i[7]``       | 23    | ps2_mouse IRQ                           |
| ``irq_fast_i[6]``       | 22    | sbi2c IRQ                               |
| ``irq_fast_i[5]``       | 21    | GPIO IRQ                                |
| ``irq_fast_i[4]``       | 20    | Quad SPI IRQ                            |
| ``irq_fast_i[3]``       | 19    | DFX Controller IRQ                      |
| ``irq_fast_i[2]``       | 18    | ICAP IRQ                                |
| ``irq_fast_i[1]``       | 17    | not assigned                            |
| ``irq_fast_i[0]``       | 16    | not assigned                            |

### The Platform Level Interrupt Controller.

One interrupt line is reserved to connect an external interrupt controller. I don't have any use for it right now, however, so I'm going to leave this unassigned for the time being.

Since we currently don't have a use for the Programmable Interrupt Controller, I'll remove it from the Architecture Diagram.

Will It Fit? Estimated FPGA Resource Utilization.
------------------------------------------------

I could keep adding modules and dream up architectures all day long, but some kind of reality-check is long overdue. I'm going to create a fork of all modules identified so far and run them through synthesis, as-is, just to get a sense of the resource utilization on the Arty A7-35T and the Nexys A7-100T. We won't get more than ballpark figures out of this, but that's all we need right now.

### Synthesis

Synthesis is handled by **Vivado**, Xilinx's FPGA Design Suite. Vivado is free to download: [https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html](https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html).

The synthesis tool turns a module's Verilog/System Verilog/VHDL source code into a netlist of gates. In the process of doing so, the tool also generates a utilization report, relative to the available resources of the target FPGA. It's this utilization report we're after right now, not the generated netlist.

Here's an example utilization report, generated during the synthesis of the MIG core:

[https://github.com/epsilon537/boxlambda/blob/main/doc/mig_7series_0_utilization_synth.rpt](https://github.com/epsilon537/boxlambda/blob/main/doc/mig_7series_0_utilization_synth.rpt)

For most of the cores, synthesis was just a matter of pointing Vivado to the core's source tree and hitting the *Run Synthesis* button. There were a few exceptions:

- VERA did not include the video, sprite, or palette RAM into the RTL source tree. I manually added those numbers into the utilization report.
- The Ibex CPU uses a build system called [FuseSoc](https://fusesoc.readthedocs.io/en/latest/). FuseSoc was easy to install and invoke. All the info was in the README for the Arty-A7 example build.
- The Ibex CPU example build for the Arty A7 includes 64KB of Block RAM. This RAM is part of the example, but not part of the CPU. I manually took that number out of the utilization report.
- I modified the Ibex CPU example to include the (M)ultiplier and (B)it Manipulation extensions. Both are parameters, set in the top-level file:

ibex_top.sv:
```
    parameter rv32m_e      RV32M            = RV32MFast,
    parameter rv32b_e      RV32B            = RV32BBalanced,
```

I organized the utilization numbers from the different cores into a table and compared them to the available resources on the Nexys A7-100T and the Arty A7-35T. The results are shown below.

### Nexys A7-100T Estimated Utilization

**BoxLambda Estimated FPGA Resource Utilization on Nexys A7-100T:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse | 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------|
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205|
|**Slice Registers**|0|1441|911|5060|622|167|185|185|
|**Block RAM Tile**|64|41|0|0|1|0.5|0|0|
|**DSPs**|0|2|1|0|0|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------|-------|--------|----------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|536|393|438|440|20.00%|17203.2|63400|27.13%|
|**Slice Registers**|324|114|346|641|20.00%|12757.2|126800|10.06%|
|**Block RAM Tile**|1|0|0|0|20.00%|129|135|95.56%|
|**DSPs**|0|0|0|0|20.00%|3.6|240|1.50%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

Overall it's an easy fit, with room to spare. All the pressure is on the Block RAM. Slice utilization (registers and combinatorial logic) is low.

### Arty A7-35T Estimated Utilization

**BoxLambda Estimated FPGA Resource Utilization on Arty A7-35T, before adjustment:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse | 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------|
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205|
|**Slice Registers**|0|1441|911|5060|622|167|185|185|
|**Block RAM Tile**|32|25|0|0|1|0.5|0|0|
|**DSPs**|0|2|1|0|0|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------|-------|--------|----------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|536|393|438|440|20.00%|17203|20800|82.71%|
|**Slice Registers**|749|324|346|641|20.00%|12757|41600|30.67%|
|**Block RAM Tile**|1|0|0|0|20.00%|71|50|**142.80%**|
|**DSPs**|0|0|0|0|20.00%|4|90|4.00%|

On the Arty A7-35T it's a tight fit. Actually, the Block RAM doesn't fit at all.
If we reduce the amount of DPRAM to 64KB and reduce the margin on Block RAM to 10%, we can just make it fit:

**BoxLambda Estimated FPGA Resource Utilization on Arty A7-35T, after adjustment:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205
|**Slice Registers**|0|1441|911|5060|622|167|185|185
|**Block RAM Tile**|**16**|25|0|0|1|0.5|0|0
|**DSPs**|0|2|1|0|0|0|0|0

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization 
|----------------|-------|-------|--------|----------|-------------|----------------------|----------------|------------------
|**Slice LUTs**|536|393|438|440|20.00%|17203|20800|82.71%
|**Slice Registers**|749|324|346|641|20.00%|12757|41600|30.67%
|**Block RAM Tile**|1|0|0|0|**10.00%**|48|50|**95.70%**
|**DSPs**|0|0|0|0|20.00%|4|90|4.00%

Slice utilization is also fairly high. This might lead to some routing issues down the line.
Still, these numbers are good enough to keep the Arty A7-35T in the running for the time being, at least as a kind of development/prototyping platform. I'm not ready yet to spend the cash on a Nexys A7-100T.

# Architecture Diagram Updates

Based on these synthesis results, I settled on the following modifications to the architecture diagrams:

- Ibex processor parameterization RV32IMCB.
- 64KB DPRAM on the Arty A7-35T.
- Replaced the PIT module on the wishbone bus with a Risc-V compliant timer implementation, close to the CPU core.
- Removed the PIC module.

![Nexys Draft Architecture Block Diagram](../assets/Nexys_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Nexys A7-100T.*


![Arty Draft Architecture Block Diagram](../assets/Arty_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Arty A7-35T.*

Interesting Links
-----------------

[https://www.linusakesson.net/scene/parallelogram/index.php](https://www.linusakesson.net/scene/parallelogram/index.php): Linus Akesson once made an FPGA-based demo. To do that, he created an FPGA-based computer, with a homegrown CPU, shader, and synthesizer. When I grow up, I want to be as cool as Linus Akesson.
