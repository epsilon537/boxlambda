---
layout: post
title: 'BoxLambda Architecture, First Draft.'
comments: true
---

In this post, we organize the key components from the previous posts into an architecture. Along the way we identify a few new components.

None of what's shown here is set in stone. The diagrams below contain some speculative pieces and there are quite a few loose ends to tie up as we get further into the project.

## The Nexys Configuration

![Nexys Draft Architecture Block Diagram](/assets/Nexys_Arch_Diagram_Draft_Post.jpg){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Nexys A7-100T.*

This is a draft architecture diagram showing the Nexys A7-100T configuration. Further down, I'll show the Arty A7-35T configuration.

### Internal RAM

The system is configured with 256KB of Dual Port RAM (DPRAM) and 128KB of Video RAM (inside the VERA module). The A7-100T has 607KB of Block RAM in total, so more than enough Block RAM should be left over for other purposes, e.g. for the *Black Box Module* (see below).

The CPU has memory mapped access to DPRAM. As a long as there are no other Bus Masters competing for access to the same bus, instructions executing from DPRAM will have a fixed cycle count.

### DMA Bus and Processor Bus

The DPRAM is hooked up to two system buses: a **DMA bus** and a **Processor bus**. Bus masters (currently only CPU and DMAC) have access to both buses as well, but the intent is that the DMA Controller uses the DMA bus for MEMC<->DPRAM transfers and the CPU uses the processor bus for DPRAM access. This intent is not hardwired into the system, however. The DMA Controller can set up transfers over the processor bus, and the processor can access external memory over the DMA bus. The two system buses are there to give bus masters some flexibility to stay out of each other's way.

Note that, besides access to external and internal memory, the DMA Controller also has access to VERA, the sound cores and the SD SPI module via the DMA bus.

Both the Processor Bus and the DMA bus are 32-bit pipelined mode Wishbone buses.

### CPU Configuration

The Ibex CPU configuration is shown as RV32IC, the I and the C indicating *Integer* and *Compressed* instruction set, respectively. I would like to include the extensions for integer multiplication and division (M) and bit manipulations (B) into the build as well. Those extensions are going to take up a considerable amount of space, however, and will also have an impact on timing closure. I'm going to defer the decision on those extensions until later, when we have more insight into this project's FPGA utilization and timing.

Note that there's no Instruction or Data Cache. Code executes directly from DPRAM or DDR. Data accesses also go straight to DPRAM or DDR.

### The Black Box, and other Reconfigurable Partitions

The Black Box Partition is an empty area in the FPGA's floorplan. This is where you can insert your own application specific logic. Do you need hardware assisted collision detection for your Bullet-Hell Shoot'em Up game? Put it in the Black Box. A DSP? A CORDIC core? More RAM? As long as it fits the floor plan, you can put in in the Black Box region. The Black Box has bus master and slave ports on both system buses.

Notice that the Black Box sits inside RP\_0, Reconfigurable Partition 0. A **Reconfigurable Partition** is a region on the FPGA where you can dynamically load a **Reconfigurable Module** (RM) into. Going back to the previous examples, the collision detector, DSP, CORDIC core, or RAM module, would be Reconfigurable Modules. You can live-load one of them into RP\_0. 

VERA and the two YM2149 cores are also placed into their own Reconfigurable Partitions (RP\_1 resp. RP\_2), so you can swap those out for a different graphics and/or sound controller.

The CPU, DMAC, MEMC, and I/O peripheral blocks are all part of the so-called *Static Design*. These can't be swapped out for other logic on a live system. Any changes in these blocks require an update of the **Full Configuration Bitstream** (as opposed to a **Partial Configuration Bitstream** containing a Reconfigurable Module).

Reconfigurable Modules require a reconfigurable clocking strategy. That's the role of the *Clock Control* (clk_ctrl) module. The BoxLambda Clocking Strategy is a topic for a future post.

### New Modules

A few new modules popped out of the woodwork:

- **PIT, IRQ & GPIO** : a placeholder for Programmable Interval Timers, an Interrupt Controller and General Purpose I/O. I haven't settled on specific modules yet. To be revisited.
- **DFX Controller** : The actual loading of an Reconfigurable Module into an Reconfigurable Partition is handled by the DFX Controller. DFX stands for **Dynamic Function Exchange** which is Xilinx-speak for Partial FPGA Reconfiguration.
- **ICAP** : Internal Configuration Access Port. This module gives access to the FPGA configuration functionality built into Xilinx FPGAs. We'll use the ICAP to implement in-system updates of the Full Configuration Bitstream, loaded into the FPGA upon boot-up.
- **Quad SPI Flash** : This is a module provided by Xilinx, giving access to the Flash Memory device attached through a Quad-SPI bus. The non-volatile Flash Memory will hold the Full Configuration Bitstream(s), System Firmware, and non-volatile system configuration parameters such as keyboard type.

### External Memory Access

The Memory Controller is equipped with an AXI4 port. That's convenient because that's also what the DFX Controller uses to fetch the Reconfigurable Modules' bitstreams. 
To hook up the system buses, we use a Wishbone to AXI bridge. This bridge will introduce additional memory access latency, but that should be acceptable because this path should not be used for latency-critical operations.

Note that the CPU has memory-mapped access to DDR and can execute code directly from DDR. DDR acceses are not fully deterministic however. CPU instructions executing from DDR will not have a fixed cycle count.

## The Arty Configuration

![Arty Draft Architecture Block Diagram](/assets/Arty_Arch_Diagram_Draft_Post.jpg){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Arty A7-35T.*

This is a draft architecture diagram showing the Arty A7-35T configuration.

DFX is not supported on the A7-35T. Neither is the Hierarchical Design Flow. This means we have to stick to a monolitic design. The RTL for all components is combined into one single design, which is synthesized, implemented, and turned into a single bitstream. There is still room for RTL experimentation in this build, but you won't be able to live-load it. It's going to require an update of the Full Configuration Bitstream.

The A7-35T FPGA has much less Block RAM than the A7-100T. As a result, the amount of video RAM has been reduced to 64KB, and the amount of DPRAM has been reduced to 128KB. 

All other components are the same as in the Nexys Configuration.

## Example Software Usage Model

Obviously, BoxLambda users can make up their own minds on how they want to set up this system. Here's one possible software configuration:

- *Deterministic* and/or Time-Critical CPU code and data reside in DPRAM.
- Non-Time-Critical code and data reside in DDR memory.
- The CPU accesses DPRAM, DDR and hardware blocks via the Processor Bus.
- DMA activity, if any, passes over the DMA bus.

## Loose Ends

This post has briefly introduced several new concepts, and also raises questions on topics not mentioned so far:

- Partial FPGA Reconfiguration (DFX, Reconfigurable Partitions, Reconfigurable Modules).
- Interrupt Handling.
- The System Boot procedure (ICAP).
- In-System Updates (Quad-SPI Flash access).
- Clock Management (clk_ctrl).
- Bus Arbitration.

All of these topics will be further explored in future posts.

Interesting links
-----------------

[https://mega65.org/](https://mega65.org/) : BoxLambda is not the own retro-style computer that runs (or rather, will run) on a Nexys A7. Who knows, maybe someday BoxLambda will fit in the super-sexy MEGA65 form factor.
