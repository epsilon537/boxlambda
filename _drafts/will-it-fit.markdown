---
layout: post
title: 'Will It Fit? Estimated FPGA Resource Utilization.'
comments: true
---

I could keep adding cool modules and dream up architectures all day long, but some kind of reality-check is long overdue. I'm going to create a fork of all modules identified so far and run them through synthesis, as-is, just to get a sense of the resource utilization on the Arty A7-35T and the Nexys A7-100T. We won't get more than ballpark figures out of this, but that's all we need right now.

# Synthesis

Synthesis is handled by **Vivado**, Xilinx's FPGA Design Suite. Vivado is free to download: [https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html](https://www.xilinx.com/products/design-tools/vivado/vivado-ml.html).

The synthesis tool turns a module's Verilog/System Verilog/VHDL source code into a netlist of gates. In the process of doing so, the tool also generates a utilization report, relative to the available resources of the target FPGA. It's this utilization report we're after right now, not the generated netlist.

Here's an example utilization report, generated during synthesis of the MIG core:

[https://github.com/epsilon537/boxlambda/blob/main/doc/mig_7series_0_utilization_synth.rpt](https://github.com/epsilon537/boxlambda/blob/main/doc/mig_7series_0_utilization_synth.rpt)

For most of the cores, synthesis was just a matter of pointing Vivado to the core's source tree and hitting the *Run Synthesis* button. There were a few exceptions:

- VERA did not include the video, sprite, or palette RAM into the RTL source tree. I manually added those numbers into the utilization report.
- The Ibex CPU uses a build system called [FuseSoc](https://fusesoc.readthedocs.io/en/latest/). FuseSoc was easy to install and invoke. All the info was in the README for the Arty-A7 example build.
- The Ibex CPU example build for the Arty A7 included 64KB of Block RAM. This RAM was part of the example, but not part of the CPU. I manually took that number out of the utilization report.
- I modified the Ibex CPU example to include the (M)ultiplier and (B)it Manipulation extensions. Both are parameters, set in the top-level file:

ibex_top.sv:
```
    parameter rv32m_e      RV32M            = RV32MFast,
    parameter rv32b_e      RV32B            = RV32BBalanced,
```

I organized the utilization numbers from the different cores into a table and compared them to the available resources on the Nexys A7-100T and the Arty A7-35T. The results are below.

# Nexys A7-100T Estimated Utilization

**BoxLambda Estimated FPGA Resource Utilization on Nexys A7-100T:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse | 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------|
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205|
|**Slice Registers**|0|1441|911|5060|622|167|185|185|
|**Block RAM Tile**|64|41|0|0|1|0.5|0|0|
|**DSPs**|0|2|1|0|0|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------|-------|--------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|536|393|438|20.00%|16675.2|63400|26.30%|
|**Slice Registers**|324|114|346|20.00%|11988|126800|9.45%|
|**Block RAM Tile**|1|0|0|20.00%|129|135|95.56%|
|**DSPs**|0|0|0|20.00%|3.6|240|1.50%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

Overall it's an easy fit, with room to spare. All the pressure is on the Block RAM. Slice utilization (registers and combinatorial logic) is low.

# Arty A7-35T Estimated Utilization

**BoxLambda Estimated FPGA Resource Utilization on Arty A7-35T, before adjustment:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse | 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------|
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205|
|**Slice Registers**|0|1441|911|5060|622|167|185|185|
|**Block RAM Tile**|32|25|0|0|1|0.5|0|0|
|**DSPs**|0|2|1|0|0|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------|-------|--------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|536|393|438|20.00%|16675.2|20800|80.17%|
|**Slice Registers**|749|324|346|20.00%|11988|41600|28.82%|
|**Block RAM Tile**|1|0|0|20.00%|71.4|50|**142.80%**|
|**DSPs**|0|0|0|20.00%|3.6|90|4.00%|

On the Arty A7-35T it's a tight fit. Actually, the Block RAM doesn't fit at all.
If we reduce the amount of DPRAM to 64KB and reduce the margin on Block RAM to 10%, we can just make it fit:

**BoxLambda Estimated FPGA Resource Utilization on Arty A7-35T, after adjustment:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse 
|----------------|--------|------|---------------|-----|------|------------|-----------|-----------
|**Slice LUTs**|0|2122|3390|5673|554|380|205|205
|**Slice Registers**|0|1441|911|5060|622|167|185|185
|**Block RAM Tile**|**16**|25|0|0|1|0.5|0|0
|**DSPs**|0|2|1|0|0|0|0|0

| Resources Type | sdspi | wbi2c | wbuart | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization 
|----------------|-------|-------|--------|-------------|----------------------|----------------|------------------
|**Slice LUTs**|536|393|438|20.00%|16675.4|20800|80.17%
|**Slice Registers**|749|324|346|20.00%|11988|41600|28.82%
|**Block RAM Tile**|1|0|0|**10.00%**|47.85|50|**95.70%**
|**DSPs**|0|0|0|20.00%|3.6|90|4.00%

Slice utilization is also fairly high. This might lead to some routing issues down the line.
Still, these numbers are good enough to keep the Arty A7-35T in the running for the time being, at least as a kind of development/prototyping platform. I'm not ready yet to spend the cash on a Nexys A7-100T.

Architecture Diagram Updates
----------------------------
Based on these synthesis results, I settled on the following modifications to the architecture diagrams:

- Ibex processor parameterization RV32IMCB.
- 64KB DPRAM on the Arty A7-35T.

![Nexys Draft Architecture Block Diagram](../assets/Nexys_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Nexys A7-100T.*


![Arty Draft Architecture Block Diagram](../assets/Arty_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Arty A7-35T.*

Interesting Links
-----------------

[https://www.linusakesson.net/scene/parallelogram/index.php](https://www.linusakesson.net/scene/parallelogram/index.php): Linus Akesson once made an FPGA-based demo. To do that, he created an FPGA-based computer, with a homegrown CPU, shader, and synthesizer. When I grow up, I want to be as cool as Linus Akesson.
