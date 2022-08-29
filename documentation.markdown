---
layout: page
title: Documentation
permalink: /documentation/
---

* TOC
{:toc}

Terms and Abbreviations
-----------------------
This section provides clarification for some of the more ambiguous terms and abbreviations used below.

- **AXI**: Advanced eXtensible Interface, ARM's SoC bus specification.

- **Bitstream**: An FPGA Bitstream is a file containing the programming data associated with an FPGA chip.

- **Blitter**: A type of DMA often used in the context of 2D graphics, copying, combining, and/or modifying bitmap graphics in video memory.

- **BPP**: Bits Per Pixel.

- **BSCANE**: A Xilinx primitive giving access to and from the FPGA's JTAG scan chain.

- **Console**: The physical terminal consisting of a screen, a keyboard, and optionally a mouse. Console I/O means input/output from/to these physically attached devices.

- **Constraints File**: A constraints file specifies the mapping of the top-level HDL module's input and output ports to physical pins of the FPGA. It also defines the clocks used by the given design. See [https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file](https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file).

- **CPU**: Central Processing Unit.

- **DAC**: Digital-to-Analog Converter.

- **DFX**: Dynamic Function Exchange, Xilinx's solution for Partial FPGA Reconfiguration ([https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf))

- **DMA**: Direct Memory Access, a hardware assist component offloading memory copy operations from the CPU.

- **DSP**: Digital Signal Processing.

- **DMAC**: DMA Controller.

- **DPRAM**: Dual-Port RAM.

- **DTM**: Debug Transport Module.

- **DUT**: Device Under Test.

- **EDA tool**: A software tool to design electronic circuits, e.g. Vivado.

- **FIFO**: First-In-First-out, an implementation of a queue.

- **Fork**: A GitHub fork is a copy of a repository that sits in your account rather than the account from which you forked the data.

- **FTDI**: Future Technology Devices International Inc. The name has become synonymous with the USB-to-UART adapter ICs sold by this company.

- **GPIO**: General-Purpose Input/Output, an uncommitted pin used for input and/or output controllable by the user at run-time.

- **Hacker/Hacking**: See [http://www.paulgraham.com/gba.html](http://www.paulgraham.com/gba.html)

- **Ibex**: The name of the Risc-V CPU core used by BoxLambda.

- **IC**: Integrated Circuit.

- **Interconnect**: Wishbone terminology for the bus fabric.

- **IP-XACT**: An XML format that defines and describes individual, re-usable electronic circuit designs to facilitate their use in creating integrated circuits.

- **IP Package**: A Vivado file encapsulating an IP component using the IP-XACT file format.

- **IRQ**: Interrupt Request.

- **ICAP**: Internal Configuration Access Port, a module giving access to the FPGA configuration functionality built into Xilinx FPGAs ([https://www.xilinx.com/products/intellectual-property/axi_hwicap.html](https://www.xilinx.com/products/intellectual-property/axi_hwicap.html))

- **ISA**: Instruction Set Architecture. The Instruction Set Architecture is the part of the processor that is visible to the programmer.

- **JTAG**: Joint Test Action Group, a standard designed to assist with device, board, and system testing, diagnosis, and fault isolation. Today JTAG is used as the primary means of accessing sub-blocks of ICs, making it an essential mechanism for debugging embedded systems.

- **JTAG DTM**: JTAG based Debug Transport Module.

- **JT49**: The name of Jotego's YM2149 compatible sound core implementation.

- **Linting**: Static Code Analysis.

- **LUT**: Look-Up Table.

- **Makefile**: A file used by the *Make* utility, defining a set of tasks to be executed, and defining dependencies between tasks. Makefiles are commonly used to create build systems.

- **Memory File**: A file containing the initial contents of a Block RAM instance used in an FPGA design.

- **MIG**: Memory Interface Generator, a parameterizable Xilinx IP module used to generate a Memory Controller.

- **MEMC**: Memory Controller.

- **OOC**: Vivado's OOC mode or OOC flow lets you synthesize, implement, and analyze design modules in a hierarchical design.

- **OpenOCD**: Open On-Chip Debugger, open-source software that interfaces with a hardware debugger's JTAG port.

- **PIT**: Programmable Interval Timer.

- **PMOD**: Peripheral Module Interface, an open standard defined by Digilent for connecting peripheral modules to an FPGA.

- **Praxos**: The name of the DMA Controller used by BoxLambda.

- **PSG**: Programmable Sound Generator.

- **PWM**: Pulse Width Modulation.

- **Repo**: Repository.

- **RP**: Reconfigurable Partition. Part of Xilinx's DFX solution.

- **RM**: Reconfigurable Module. Part of Xilinx's DFX solution.

- **RTL**: Register-Transfer Level, an abstraction of a Digital Design, usually captured using a Hardware Description Language such as Verilog, SystemVerilog, or VHDL.

- **RV32IMCB**: Risc-V 32-bit Processor Variant with Multiplier/Divider, Compressed ISA, and Bit Manipulating Extensions.

- **Slice**: The basic logical unit of a Xilinx FPGA.

- **(Software) Image**: Snapshot of computer memory contents stored as a file.

- **SoC**: System-on-a-Chip. A System-on-a-Chip is an integrated circuit that integrates all or most components of a computer or other electronic system.

- **SPI**: Serial Peripheral Interface, a synchronous serial communication interface specification used for short-distance communication.

- **Synthesis**: Synthesis turns a module's Verilog/System Verilog/VHDL source code into a netlist of gates. The software equivalent of synthesis is compilation.

- **TAP**: Test Access Port, a JTAG interface.

- **Tcl**: The defacto standard embedded command language for EDA applications.

- **USB HIB**: USB Human Interface device Class, a part of the USB specification for computer peripherals such as keyboards and mice.

- **VCS**: Version Control Subsystem.

- **Verilate**: To compile HDL to C++ using Verilator.

- **Verilator**: An HDL to C++ compiler.

- **VERA**: Versatile Embedded Retro Adapter, the name of the graphics core used by BoxLambda.

- **VRAM**: Video RAM.

- **WIP**: Work In Progress.

- **Wishbone**: An Open-Source SoC bus specification: [https://cdn.opencores.org/downloads/wbspec_b4.pdf](https://cdn.opencores.org/downloads/wbspec_b4.pdf).

- **WSL**: Windows Subsystem for Linx.

- **Xbar**: Cross-Bar, a type of interconnect used in SoC bus fabrics.

- **YM2149**: An '80s era Yamaha sound chip. See also JT49.

Goals
-----

- Create a sandbox for experimenting with software and (FPGA) hardware.    
    - **Simplicity**: It should be easy to jump in and do something: create, hack, tinker.
        - It should be doable for a single person to develop a good understanding of the entire system, software and hardware.
        - **Deterministic Behavior**: By design, it should be clear how long an operation, be it an instruction or a DMA transfer, is going to take.
        - **Single User/Single Tasking OS** booting to a console shell.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components.
        - Support for **partial FPGA reconfiguration**.
- Target Hardware is Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start) and/or the [Nexys-A7](https://digilent.com/reference/programmable-logic/nexys-a7/start).
- The computer should support the following peripherals:
  - Keyboard
  - Mouse (optional)
  - Joystick (optional)
  - Serial port
  - SD card storage
  - VGA Display
  - Audio output
  
- Sound and graphics should be sufficient to support retro-style 2D gameplay.

Requirement Analysis
--------------------

### Simplicity

Simplicity will be a strong guideline when making design choices. For instance, it may mean that we decide against a popular-but-complex processor in favor of a more obscure-but-simple processor.

It is hard to make something simple. The Simplicity requirement will make system design harder, not easier. For a case in point, see below.

#### Deterministic Behavior

Designing a deterministic system is more complex than designing a system that allows some slack in the completion of operations. However, once such a system is in place, it becomes much easier to reason about it and design applications on top of it, especially applications with real-time requirements.
For instance, it would be pretty cool if the system is designed so that racing-the-beam becomes possible, i.e. time actions within an application's main loop so that they take place on a specific screen scan line and a specific column on that scan line. Think Commodore 64 split raster bars and sprite multiplexing.

Note that deterministic behavior must be guaranteed only when required by the application. Less deterministic operations are perfectly acceptable when the application does not require full deterministic behavior. E.g. a deterministic application runs from Block RAM with known, fixed memory access latency, while a non-deterministic application may run from bursty external memory.

One consequence of the *Deterministic Behavior* requirement is that bus arbitration should be done using fixed time slots to be able to guarantee fixed timing, latency, and bandwidth to each bus master.

#### Single User / Single Tasking OS

We won't be running Linux or any other multitasking OS for that matter. The platform will only run one application at a time and that application will be fully in charge of the entire system.

A Single User / Single Tasking OS will provide the following services:

- A console CLI shell allowing user and scripted access to:

	- navigate the file system
	- load/save software images to/from memory
	- copy/move/delete files
	- execute (transfer control to) applications in memory, optionally passing in command-line arguments
	- peeking and poking into memory
	
- File System I/O kernel routines
- Console I/O kernel routines: Input from a physically attached keyboard, output to a physically attached screen.
- UART I/O kernel routines
- Discovery and enumeration of hardware components. See Modular Architecture below.

#### Not Boot-to-Basic

I don't want to be pinned down to, or give preference to, any particular interpreted language, so we're not going going to Boot-to-Basic.
We're not going for full-retro boot-to-Basic.

I would like to allow open support for multiple interpreted languages by letting the application image indicate in which language it's written, e.g. by specifying on the first line the path to the interpreter to use, as commonly used in Linux scripting: *#!/usr/bin/python, #!/usr/bin/ulisp, ...*

It should also be possible to directly execute binary images of course.

### Modular Architecture

I imagine a reference configuration to which hardware components can be added or from which components can be removed.
Applications should be able to discover, with the help of the OS, whether a certain component is present or not.

#### Partial FPGA Reconfiguration

It would be very cool if a hardware component can be incrementally loaded into the FPGA, using Xilinx's *DFX* (Dynamic Function eXchange) feature. This would allow applications to be packaged along with specific hardware components (e.g. accelerators or peripherals) on which they depend.

I'm considering this feature a stretch goal for the project.

### Target Hardware and Peripherals

I currently have an **Arty A7 35T**, with the following PMODs for peripherals:

- [Pmod MicroSD: microSD Card Slot](https://digilent.com/shop/pmod-microsd-microsd-card-slot/)
- [Pmod PS2: Keyboard/mouse connector](https://digilent.com/shop/pmod-ps2-keyboard-mouse-connector/) 
- [Pmod AMP2: Audio Amplifier](https://digilent.com/shop/pmod-amp2-audio-amplifier/) 
- [Pmod VGA: Video Graphics Array](https://digilent.com/shop/pmod-vga-video-graphics-array/) 
- [Wii Nunchuck Adapter](https://www.reichelt.com/be/en/arduino-8211-wiichuck-nunchuck-adapter-ard-wii-nunchuck-p282673.html?CCOUNTRY=661&LANGUAGE=nl&GROUPID=9020&START=0&OFFSET=16&SID=93757c8e4582e90848068d74dbb71d4a2c938ebd13432dc6b9c96&LANGUAGE=EN&&r=1)

I suspect that over time the project will outgrow this setup and I might move up to the **Nexys A7-100T**, also from Diligent. Compared to the Arty A7 35T, Nexys A7-100T has: 
- A bigger FPGA: More logic slices and more Block RAM.
- Onboard microSD card connector
- Onboard PWM audio output connector
- Onboard PDM microphone connector
- USB HID for keyboard and mouse, with a clever adapter so keyboard and mouse present themselves to the FPGA as PS/2 devices.
- VGA connector

Architecture
------------

### The Nexys Configuration

![Nexys Draft Architecture Block Diagram](assets/Nexys_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Nexys A7-100T.*

This is an architecture diagram showing the Nexys A7-100T configuration. Further down, I'll show the Arty A7-35T configuration.

#### Internal RAM

The system is configured with 256KB of Dual-Port RAM (DPRAM) and 128KB of Video RAM (inside the VERA module). The A7-100T has 607KB of Block RAM in total, so more than enough Block RAM should be left over for other purposes, e.g. for the *Black Box Module* (see below).

The CPU has memory-mapped access to DPRAM. As long as no other Bus Masters are competing for access to the same bus, instructions executing from DPRAM will have a fixed cycle count.

#### DMA Bus and Processor Bus

The DPRAM is hooked up to two system buses: a **DMA bus** and a **Processor bus**. Bus masters (currently only CPU and DMAC) have access to both buses as well, but the intent is that the DMA Controller uses the DMA bus for MEMC<->DPRAM transfers and the CPU uses the processor bus for DPRAM access. This intent is not hardwired into the system, however. The DMA Controller can set up transfers over the processor bus, and the processor can access external memory over the DMA bus. The two system buses are there to give bus masters some flexibility to stay out of each other's way.

Note that, besides access to external and internal memory, the DMA Controller also has access to VERA, the sound cores, and the SD SPI module via the DMA bus.

Both the Processor Bus and the DMA bus are 32-bit pipelined mode Wishbone buses.

#### The Interconnect

A bus on a block diagram is just a line connecting blocks. In reality, the *Interconnect* consists of Cross Bars, Arbiters, Address Decoders, and Bridges. I will follow up with an architecture diagram showing the BoxLambda Interconnect details. 

To build the Interconnect, I will make use of the components contributed by the gentlemen below:

- **Alexforencich** published a collection of components that can be used to build an Interconnect: [https://github.com/alexforencich/verilog-wishbone/](https://github.com/alexforencich/verilog-wishbone/)
- **ZipCPU** did the same. His components are well-documented, including cross-references with insightful articles on the ZipCPU website: [https://github.com/ZipCPU/wb2axip](https://github.com/ZipCPU/wb2axip)

#### The Black Box, and other Reconfigurable Partitions

The Black Box Partition is an empty area in the FPGA's floorplan. This is where you can insert your application-specific logic. Do you need hardware-assisted collision detection for your Bullet-Hell Shoot'em Up game? Put it in the Black Box. A DSP? A CORDIC core? More RAM? As long as it fits the floor plan, you can put it in the Black Box region. The Black Box has bus master and slave ports on both system buses.

Notice that the Black Box sits inside RP\_0, Reconfigurable Partition 0. A **Reconfigurable Partition** is a region on the FPGA where you can dynamically load a **Reconfigurable Module** (RM) into. Going back to the previous examples, the collision detector, DSP, CORDIC core, or RAM module, would be Reconfigurable Modules. You can live-load one of them into RP\_0. 

VERA and the two YM2149 cores are also placed into their specific Reconfigurable Partitions (RP\_1 resp. RP\_2), so you can swap those out for a different graphics and/or sound controller.

The CPU, DMAC, MEMC, and I/O peripheral blocks are all part of the so-called *Static Design*. These can't be swapped out for other logic on a live system. Any changes in these blocks require an update of the **Full Configuration Bitstream** (as opposed to a **Partial Configuration Bitstream** containing a Reconfigurable Module).

Reconfigurable Modules require a reconfigurable clocking strategy. That's the role of the *Clock Control* (clk_ctrl) module. The BoxLambda Clocking Strategy is a topic for a future post.

#### External Memory Access

The Memory Controller is equipped with an AXI4 port. That's convenient because that's also what the DFX Controller uses to fetch the Reconfigurable Modules' bitstreams. 
To hook up the system buses, we use a Wishbone to AXI bridge. This bridge will introduce additional memory access latency, but that should be acceptable because this path should not be used for latency-critical operations.

Note that the CPU has memory-mapped access to DDR memory and can execute code directly from DDR memory. DDR memory access is not fully deterministic, however. CPU instructions executing from DDR will not have a fixed cycle count.

### The Arty A7 Configuration

![Arty Draft Architecture Block Diagram](assets/Arty_Arch_Diagram_Doc.png){:class="img-responsive"}
*BoxLambda Draft Architecture Block Diagram for Arty A7-35T.*

This architecture diagram shows the Arty A7-35T configuration.

DFX is not supported on the A7-35T. Neither is the Hierarchical Design Flow. This means we have to stick to a monolithic design. The RTL for all components is combined into one single design, which is synthesized, implemented, and turned into a single bitstream. There is still room for RTL experimentation in this build, but you won't be able to live-load it. It's going to require an update of the Full Configuration Bitstream.

The A7-35T FPGA has much less Block RAM than the A7-100T. As a result, the amount of video RAM and the amount of DPRAM have been reduced to 64KB. 

All other components are the same as in the Nexys Configuration.

### Example Software Usage Model

BoxLambda users can make up their minds on how they want to set up this system. Here's one possible software configuration:

- *Deterministic* and/or Time-Critical CPU code and data reside in DPRAM.
- Non-Time-Critical code and data reside in DDR memory.
- The CPU accesses DPRAM, DDR memory, and hardware blocks via the Processor Bus.
- DMA activity, if any, passes over the DMA bus.

Interrupts
----------

The CPU supports the following interrupts (taken from [https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html)):

**Ibex Interrupts:**

| Interrupt Input Signal  | ID    | Description                                      |
-------------------------|-------|--------------------------------------------------|
| ``irq_nm_i``            | 31    | Non-maskable interrupt (NMI)                     |
| ``irq_fast_i[14:0]``    | 30:16 | 15 fast, local interrupts                        |
| ``irq_external_i``      | 11    | Connected to platform-level interrupt controller |
| ``irq_timer_i``         | 7     | Connected to timer module                        |
| ``irq_software_i``      | 3     | Connected to memory-mapped (inter-processor)     |
|                         |       | interrupt register                               |

### The Timer

The RISC-V spec includes a timer specification: RISC-V Machine Timer Registers (see RISC-V Privileged Specification, version 1.11, Section 3.1.10). The Ibex GitHub repository contains a compliant implementation as part of the *Simple System* example:

[https://github.com/epsilon537/ibex/tree/master/examples/simple_system](https://github.com/epsilon537/ibex/tree/master/examples/simple_system)

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

Estimated FPGA Utilization
--------------------------

**Estimated FPGA Resource Utilization on Nexys A7-100T:**


| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | riscv-dbg | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse | 
|----------------|--------|------|---------------|-----------|------|------------|-----------|-----------|
|**Slice LUTs**|0|2122|3390|5673|416|554|380|205|205|
|**Slice Registers**|0|1441|911|426|5060|622|167|185|185|
|**Block RAM Tile**|64|41|0|0|1|0.5|0|0|
|**DSPs**|0|2|1|0|0|0|0|0|

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization |
|----------------|-------|-------|--------|----------|-------------|----------------------|----------------|------------------|
|**Slice LUTs**|536|393|438|440|20.00%|17702|63400|27.92%|
|**Slice Registers**|324|114|346|641|20.00%|13268|126800|10.46%|
|**Block RAM Tile**|1|0|0|0|20.00%|129|135|95.56%|
|**DSPs**|0|0|0|0|20.00%|3.6|240|1.50%|

I added a 20% margin overall for the bus fabric and for components I haven't included yet.

**Estimated FPGA Resource Utilization on Arty A7-35T:**

| Resources Type |  DPRAM | Vera | Ibex RV32IMCB | riscv-dbg | MIG | Dual JT49 | Praxos DMA | ps2 keyb. | ps2 mouse 
|----------------|--------|------|---------------|-----------|------|------------|-----------|-----------
|**Slice LUTs**|0|2122|3390|5673|416|554|380|205|205
|**Slice Registers**|0|1441|911|426|5060|622|167|185|185
|**Block RAM Tile**|**16**|25|0|0|1|0.5|0|0
|**DSPs**|0|2|1|0|0|0|0|0

| Resources Type | sdspi | wbi2c | wbuart | Quad SPI | Margin Pct. | Total (incl. margin) | Avl. Resources | Pct. Utilization 
|----------------|-------|-------|--------|----------|-------------|----------------------|----------------|------------------
|**Slice LUTs**|536|393|438|440|20.00%|17702|20800|85.11%
|**Slice Registers**|749|324|346|641|20.00%|13268|41600|31.90%
|**Block RAM Tile**|1|0|0|0|**10.00%**|48|50|**95.70%**
|**DSPs**|0|0|0|0|20.00%|4|90|4.00%

Components
----------

We're building a System-on-a-Chip (*System-on-an-FPGA*?). This section identifies the Key Components of the BoxLambda SoC.

### The Bus

The Bus, or interconnect, is the fabric stitching together the SoC internal components. For this project, the two most relevant SoC internal bus specifications are [ARM's AXI bus](https://developer.arm.com/documentation/ihi0022/latest) and the Open-Source [Wishbone bus](https://wishbone-interconnect.readthedocs.io/en/latest/).

AXI is very powerful, very popular, and very complex. It scales up well to very big SoCs. However, I don't think it scales down very well to simple SoCs, such as BoxLambda, where low latency and low complexity are more important than high bandwidth and scalability. Hence, for this project, I'm electing to go with **Wishbone**. 

We'll be using the [Wishbone B4 specification](https://github.com/fossi-foundation/wishbone/blob/master/documents/spec/wbspec_b4.pdf).

Sticking to a well-defined internal bus specification certainly helps to meet the Modular Architecture Requirement. Whether we can also accommodate Partial FPGA Reconfiguration using a Wishbone Interconnect remains to be seen.

### The Processor

#### Processor Word Size

Typical processor word sizes are 8-bit, 16-bit, 32-bit, and 64-bit. Which word size is the best fit for Boxlambda?

- **8-bit**: A *good* word size. 
  - *Pros*: 
	- An 8-bit word (i.e. a *byte*) is a good natural fit for a pixel value, an ASCII character code, or small integer values. 
	- 8-bit processors, their programs, and their data are very compact. 
	- 8-bit processors side-step some of the alignment issues seen with larger word sizes.
  - *Cons*: 
	- An 8-bit word is too small to conveniently hold the values you need in a typical program - think calculations and table indices. 
	- Toolchain support for higher-level languages is limited.
- **16-bit**: A clumsy compromise between 8-bit and 32-bits. Made sense when 32-bit processors were not readily available yet. Now, not so much.
- **32-bit**: Another *good* word size.
  - *Pros*: 32-bit words can hold most real-world numbers and cover a huge address space. 32-bit machines generally have good toolchain support.
  - *Cons*: Much bigger than its 8-bit counterpart, in terms of FPGA real estate, program size as well as data size.
- **64-bit**: A big and clunky word size, way too big to handle conveniently, intended for specialized use cases that don't fit this project.

I've decided to go for a **32-bit processor**. A 32-bit processor (and associated on-chip memory) will take a bigger chunk out of our FPGA real estate, but I think it's worth it. I like the convenience of 32-bit registers, and a 32-bit processor may come with a regular GCC toolchain.

#### Processor Features

Next to a 32-bit word size, we're looking for the following features for our microprocessor: 

- Ease of programming, meaning:
  - Easy and well-documented *Instruction Set Architectures* (ISA). We want to be able to program the machine at assembly language level.
  - Shallow Pipeline: It is relatively easy to reason about the behavior of a processor with a two-stage pipeline. It is not very easy to reason about the behavior of a processor with a six-stage pipeline.
  - Good toolchain support, such as GCC, so we can build a software ecosystem for our machine.
- An accessible and well-documented implementation.
- Has to fit our FPGA, with enough space to fit the other components.
  
With all that in mind, I think [**RISC-V**](https://riscv.org/) is a great option.

- Great ISA, building on lessons learned from previous popular processor architectures.
- 32-bit support.
- GCC toolchain support.
- Open-Source.
- Well-documented.
- Very fashionable. Let's ride that wave :-)

#### The CPU core: Ibex

There are a lot of RISC-V implementations to choose from. The **Ibex** project seems like a good choice:

[https://github.com/lowRISC/ibex](https://github.com/lowRISC/ibex)

- 32-bit RISC-V.
- High-quality, well-documented implementation.
- SystemVerilog based. My preferred HDL.
- Supports a *small* two-stage pipeline parameterization.
- Very active project.

Location of the *Ibex* submodule in the BoxLambda repo: **sub/ibex/**

The Ibex core:
[sub/ibex/rtl/ibex_top.sv](https://github.com/epsilon537/ibex/blob/acdf41b2bf3ed2f33ed5c29e65c1625d22e4aab5/rtl/ibex_top.sv)

#### The Wishbone Wrapper: *Ibex_WB*

[https://github.com/epsilon537/ibex_wb](https://github.com/epsilon537/ibex_wb) forked from [https://github.com/batuhanates/ibex_wb](https://github.com/batuhanates/ibex_wb)

The Ibex RISCV core itself doesn't have Wishbone ports. *Ibex_WB* wraps around the vanilla Ibex core and attaches Wishbone port adapters to its instruction and data ports.

The *Ibex_WB* repo also includes an example SoC build consisting of an Ibex core connected via a shared Wishbone bus to a wbuart32 core and an internal memory module, along with the software to run on that platform. This example SoC is the starting point for BoxLambda's implementation. See the **Test Builds** section below.

Location of the *Ibex_WB* submodule in the BoxLambda repo: **sub/ibex/**

The *wb_ibex_core*:
[sub/ibex_wb/rtl/wb_ibex_core.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/rtl/wb_ibex_core.sv)

#### Ibex Core Configuration

I settled on RISCV configuration **RV32IMCB**: The **(I)nteger** and **(C)ompressed** instruction set are fixed in Ibex. **(M)ultiplication and Division** and **(B)it Manipulation** are enabled optional extensions.
Note that there's no Instruction or Data Cache. Code executes directly from DPRAM or DDR memory. Data access also goes straight to DPRAM or DDR memory.
The Ibex core is instantiated with the following *M* and *B* parameters, as shown in the *Ibex_WB* *ibex_soc* example:

**sub/ibex_wb/soc/fpga/arty-a7-35/rtl/ibex_soc.sv**:
```
wb_ibex_core #(
  .RV32M(ibex_pkg::RV32MFast),
  .RV32B(ibex_pkg::RV32BBalanced)
  ,,,
```

### The Debug Unit: *RISCV-DBG*

[https://github.com/epsilon537/riscv-dbg](https://github.com/epsilon537/riscv-dbg) forked from [https://github.com/pulp-platform/riscv-dbg](https://github.com/pulp-platform/riscv-dbg).

#### RISCV OpenOCD

OpenOCD is an open-source software package used to interface with a hardware debugger's JTAG port via one of many transport protocols. In our case, the hardware debug logic is implemented by a component called **riscv-dbg**. The overall setup looks like this:

![OpenOCD General Setup](../assets/OpenOCD_Setup_General.drawio.png){:class="img-responsive"}

*OpenOCD General Setup*

BoxLambda uses the RISCV fork of OpenOCD: [https://github.com/riscv/riscv-openocd](https://github.com/riscv/riscv-openocd)

#### The RISCV-DBG component

RISCV-dbg is part of the [*PULP* platform](https://github.com/pulp-platform) and depends on three additional GitHub repositories that are part of this platform:
- **common_cells**: [https://github.com/pulp-platform/common_cells](https://github.com/pulp-platform/common_cells)
- **tech_cells_generic**: [https://github.com/pulp-platform/tech_cells_generic](https://github.com/pulp-platform/tech_cells_generic)
- **pulpino**: [https://github.com/pulp-platform/pulpino](https://github.com/pulp-platform/pulpino)

As their names suggest, *common_cells* and *tech_cells_generic* provide commonly used building blocks such as FIFOs, CDC logic, reset logic, etc. *Pulpino* is an entire RISCV-based SoC project. However, the riscv-dbg pulpino dependency is limited to just a few cells for clock management.

I created git submodules for all of these repositories under the BoxLambda repository's *sub/* directory. I then created a riscv-dbg component directory with a Bender.yml manifest in it, referencing all the sources needed from those submodules: [components/riscv-dbg/Bender.yml](https://github.com/epsilon537/boxlambda/blob/951bca262fe66fa7433dd0282e7a0d52e93fac6b/components/riscv-dbg/Bender.yml).

```
boxlambda
├── components
│   └── riscv-dbg
│       └── Bender.yml
└── sub
    ├── common_cells
    ├── tech_cells_generic
    ├── pulpino	
    └── riscv-dbg

```

#### RISCV-DBG RTL Structure

RISCV-DBG has two top-levels:
- [sub/riscv-dbg/src/dm_top.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dm_top.sv)
- [sub/riscv-dbg/src/dmi_jtag.sv](https://github.com/epsilon537/riscv-dbg/blob/b241f967f0dd105f7c5e020a395bbe0ec54e40e4/src/dmi_jtag.sv)

Recall that BoxLambda uses a Wishbone interconnect. The Ibex_WB submodule implements a Wishbone wrapper for the Ibex RISCV core. It does the same for RISCV-DBG's *dm_top*:  
[sub/ibex_wb/rtl/wb_dm_top.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/rtl/wb_dm_top.sv)

Refer to the *ibex_soc* example to see how RISCV-DBG is instantiated:  
[sub/ibex_wb/soc/fpga/arty-a7-35/rtl/ibex_soc.sv](https://github.com/epsilon537/ibex_wb/blob/87a97e38f3cf15bee80eb69bfa82166c00842b1e/soc/fpga/arty-a7-35/rtl/ibex_soc.sv)

#### OpenOCD and RISCV-DBG on Verilator

The JTAG transport protocol used on the Verilator Model is a simple socket-based protocol called **Remote Bitbang**.
The remote bitbang spec is just one page: 

[https://github.com/openocd-org/openocd/blob/master/doc/manual/jtag/drivers/remote_bitbang.txt](https://github.com/openocd-org/openocd/blob/master/doc/manual/jtag/drivers/remote_bitbang.txt)

The Verilator setup looks like this:

![BoxLambda OpenOCD Verilator Setup](../assets/OpenOCD_Setup_Verilator.drawio.png){:class="img-responsive"}

*BoxLambda OpenOCD Verilator Setup*

The **hello_dbg** project (directory *projects/hello_dbg/*) implements the OpenOCD Verilator setup shown above. The project contains the Hello World test build extended with the riscv-dbg component.
The project directory also contains a [test script](https://github.com/epsilon537/boxlambda/blob/2a32d1f4100204f9df4e17ba7c6e4656afff8c47/projects/hello_dbg/test/test.sh) that goes through the following steps:
1. Start the Verilator model
2. Connect OpenOCD to the model
3. Connect GDB to OpenOCD (and thus to the model)
4. Execute a UART register dump on the target
5. Check the UART register contents against expected results. 

```
boxlambda
├── projects
│   └── hello-dbg
│       ├── Bender.yml
│       ├── sim
│       │   ├── sim_main.cpp
│       │   └── sim_main.sv
│       └── test
│           ├── test.sh
│           └── test.gdb 
├── components
│   └── riscv-dbg
└── sub
    ├── common_cells
    ├── tech_cells_generic
    ├── pulpino	
    └── riscv-dbg

```

The OpenOCD configuration file for JTAG Debugging on Verilator is checked into the *openocd* directory:  
[openocd/verilator_riscv_dbg.cfg](https://github.com/epsilon537/boxlambda/blob/f696f21b3e50f66678f4e32806a65abdbdf42455/openocd/verilator_riscv_dbg.cfg)

To summarize:

1. The above OpenOCD config file is used to connect to the JTAG TAP of a Verilator model.
2. The JTAG TAP is implemented by a riscv-dbg core connected to an Ibex RISCV32 core.
2. The JTAG TAP is used to debug the software running on the Ibex RISCV32 core.
3. The JTAG TAP is accessed using a socket-based OpenOCD transport protocol called **remote_bitbang**.

See the *Test Builds* section below for the steps needed to set up an OpenOCD JTAG debug session on Verilator.

#### OpenOCD and RISCV-DBG on Arty-A7 FPGA

The obvious approach would be to bring out the JTAG signals to PMOD pins and hook up a JTAG adapter. However, there's an alternative method that doesn't require a JTAG adapter. The riscv-dbg JTAG TAP can be hooked into the FPGA scan chain which is normally used to program the bitstream into the FPGA. On the Arty-A7, bitstream programming is done using the FTDI USB serial port, so no special adapters are needed.

The riscv-dbg codebase lets you easily switch between a variant with external JTAG pins and a variant that hooks into the FPGA scan chain, by changing a single file:
- **dmi_jtag_tap.sv**: hooks up the JTAG TAP to external pins
- **dmi_bscane_tap.sv**: hooks the JTAG TAP into the FPGA scan chain. The Xilinx primitive used to hook into the scan chain do this is called BSCANE. Hence the name.

Both files implement the same module name (*dmi_jtag_tap*) and the same module ports, so you can swap one for the other without further impact on the system. Lightweight polymorphism.

On the OpenOCD side, the transport protocol for this Debug-Access-via-FPGA-scan-chain-over-FTDI is anti-climactically called **ftdi**.

![BoxLambda OpenOCD Arty A7 FTDI Setup](../assets/OpenOCD_Setup_Arty_A7.drawio.png){:class="img-responsive"}

*BoxLambda OpenOCD Arty A7 FTDI Setup*

The OpenOCD configuration file for JTAG Debugging on Arty A7 is checked into the *openocd* directory:  
[openocd/digilent_arty_a7.cfg](https://github.com/epsilon537/boxlambda/blob/102233debcb1e632e6a36c31a836c7619aaf8b29/openocd/digilent_arty_a7.cfg) 

To summarize:

1. The above OpenOCD config file is used to connect to the JTAG TAP of a riscv-dbg core...
2. ...to debug the software running on a connected Ibex RISCV32 core.
3. The riscv-dbg core's JTAG TAP is hooked into the Arty-A7's scan chain, normally used for loading a bitstream into the FPGA.
4. The Arty-A7 FPGA scan chain is accessible through the board's FTDI-based USB serial port.
5. The OpenOCD transport protocol name for this type of connection is **ftdi**.

See the *Test Builds* section below for the steps needed to set up an OpenOCD JTAG debug session on the Arty A7.

### The Memory Controller

SDRAM memory access is pretty complicated. Memory access requests get queued in the memory controller, scheduled, and turned into a sequence of commands that vary in execution time depending on the previous memory locations that were recently accessed. 

There exists a class of memory controllers, called **Static Memory Controllers**, that absorb these complexities and by design create a fixed schedule for a fixed use case, resulting in very predictable behavior. Static Memory Controllers are far off the beaten path, however. **Dynamic Memory Controllers** are more common. Dynamic Memory Controllers can handle a variety of use cases with good performance *on average*. Unfortunately, they sacrifice predictability to achieve this flexibility.

Ideally, we would use an accessible, well-documented, open-source, static memory controller design. Unfortunately, I can't find one. Rolling our own is not an option either. Doing so would require so much specific know-how, that it would kill this project. Pragmatically, our best option is to use Xilinx's [**Memory Interface Generator** (MIG)](https://docs.xilinx.com/v/u/1.0-English/ug586_7Series_MIS) with the Arty A7 (or Nexys A7) parameters as [published by Diligent](https://github.com/Digilent/Arty/tree/master/Resources/Arty_MIG_DDR3?_ga=2.230252508.1917430070.1649263055-373952187.1630942771).

The Xilinx memory controller falls squarely into the Dynamic Memory Controller class. How do we fit this into a platform that requires deterministic behavior? I think the best approach is to use a DMA engine to transfer data between SDRAM and on-chip memory. Fixed memory access latency to on-chip memory (from any bus master that requires it) can be guaranteed using an arbiter. We'll revisit this topic when we're discussing Boxlambda's architecture.

### The Graphics Subsystem

If you're reading this, you must be into the build-your-own-computer thing, which probably means you're aware of the super cool [Commander X16](https://www.commanderx16.com) project. Frank van de Hoef created the very elegant **VERA** (Video Embedded Retro Adapter) module for the X16. Here's a high-level specification, taken from the Commander X16 website:

VERA module specifications:

- Video generator featuring:
  - Multiple output formats (VGA, NTSC Composite, NTSC S-Video, RGB video) at a fixed resolution of 640x480@60Hz
  - Support for 2 layers, both supporting:
	- 1/2/4/8 bpp tile and bitmap modes
	- Support for up to 128 sprites (with inter-sprite collision detection).
  - Embedded video RAM of 128 KB.
  - Palette with 256 colors selected from a total range of 4096 colors.
- 16-channel stereo Programmable Sound Generator with multiple waveforms (Pulse, Sawtooth, Triangle, Noise)
- High-quality PCM audio playback from a 4 KB FIFO buffer featuring up to 48kHz 16-bit stereo sound.
- SecureDigital storage.

Other features, not mentioned in the blurb, include: 

- Fractional display scaling (scaling lower resolutions up to the 640x480 display resolution).
- Horizontal and Vertical smooth scrolling

Lucky for us, Frank recently released the VERA Verilog code under the generous MIT license. You can find the code here: 

[https://github.com/fvdhoef/vera-module](https://github.com/fvdhoef/vera-module)

I'm not particularly interested in VERA's PSG (Programmable Sound Generator), or the non-VGA output formats, so I might remove those from the build.

The 128KB of video RAM will take a big chunk out of our available Block RAM resources, but it'll be worth it. We're getting a lot of bang for our buck.

Note that the VERA is designed as a separate FPGA with a SPI slave interface. Some modifications will be required to integrate it into our SoC.

### Sound

A sound core is a perfect candidate for Partial FPGA Reconfiguration. There are a lot of options (Wave-Table synthesis, FM synthesis, PSG...) and a lot of open-source cores available. It would be pretty cool if the software application can just download its synthesizer of choice as part of the program.

Pretty much any core developed by [Jotego](https://github.com/jotego) sounds like a great idea.

Technically, I don't have to select a sound core. We already have sound through VERA's PCM audio playback. I'm going to select a sound core anyway because I like retro sounds and I'd like to mess around a bit with one of the old-school PSG chips. 

I think I'll go for a dual [**YM2149**](https://en.wikipedia.org/wiki/General_Instrument_AY-3-8910), one for music, one for sound FX, in a game context. The YM2149 was the Atari ST's sound chip, so we'll have a large music and sound FX archive at our disposal. Jotego developed an FPGA clone of the YM2149, the JT49:

[https://github.com/jotego/jt49](https://github.com/jotego/jt49)

### DMA

I was on the fence for a while, deciding whether or not I should include a DMA engine in our machine. In a previous post, I said I would use DMA to move data between external and internal memory. However, a DMA Controller is by definition a bus master, and having multiple bus masters (DMAC and CPU) adds significant complexity to the architecture: access to shared buses and slaves, impact on timing, etc. In a system with only one bus master, the CPU, you don't have to worry about any of that.

Then I snapped out of it and remembered that BoxLambda is intended to be a platform for RTL experimentation. It would be silly to restrict these RTL experiments to bus slave components only. In other words, the BoxLambda architecture is going to have to accommodate bus masters, so we might as well include a DMA Controller.

Some use cases for DMA in the scope of our computer include:

- Moving data between external (DDR) and internal (Block RAM) memory.
- Streaming from memory to the audio DAC.
- Blitting, i.e. copying data into video memory, taking into account the video memory's organization. For instance, copying a rectangular block of data into a frame buffer requires striding between rows of pixel data. Another example: Bit planes with 1, 2, or 4 bits-per-pixel color depths require barrel shifting when copying data to a specific pixel offset.

I spent many hours online searching for DMA Controllers. I was a bit surprised that there were so few options, so I kept digging. I found [ZipCPU's](https://github.com/ZipCPU/zipcpu/blob/master/rtl/peripherals/wbdmac.v), [FreeCore's](https://github.com/stffrdhrn/wb_dma), and [Ant Micro's](https://github.com/antmicro/fastvdma) DMA controllers. The Anti Micro DMAC seemed to be the most interesting option, with two Wishbone ports, pipelined mode, striding support, and support for any byte boundary alignment.

I was ready to go with the Ant Micro selection when I happened across an old post on Reddit where somebody proposed a 'smart' DMA concept: a DMAC with a tiny CPU embedded in it. That sounded like a great concept, so I pinged the author to check what became of his idea. In response, the author generously decided to release his code on GitHub! The core is called **Praxos**. Here is the repository:

[https://github.com/esherriff/Praxos](https://github.com/esherriff/Praxos)

Praxos has a tiny CPU with a small amount of program and data memory embedded in the core, allowing you to write microcode specifying the DMA behavior you want: word/non-word alignment, incrementing/decrementing/non-incrementing source and/or destination address, strides between transfers, combining sources, barrel shifting... Maximum flexibility!

It's not perfect though. Praxos only has one bus master port, an Avalon port at that. It should be doable to slap a standard Wishbone port onto it, but in its current form, I think it won't be able to take advantage of Wishbone's pipelined burst mode. That's unfortunate for a DMAC. 

Still, having the option to hack together my own application-specific DMA microcode sounds like a lot of fun. I just have to go with the Praxos option.

Many thanks to esherriff for making his code available!

### Storage

I'm going to use ZipCPU's SD Card Controller in combination with the FatFs software library to mount a FAT filesystem on the SD card:

- SD Card Controller: [https://github.com/ZipCPU/sdspi](https://github.com/ZipCPU/sdspi)
- FatFs library: [http://elm-chan.org/fsw/ff/00index_e.html](http://elm-chan.org/fsw/ff/00index_e.html)

The SD Card Controller has a Wishbone slave port.

### Keyboard and Mouse

FreeCores has PS/2 keyboard and mouse modules: [https://github.com/freecores/ps2](https://github.com/freecores/ps2)

These cores don't have a Wishbone slave port, so we're going to have to add that ourselves.

Note that the Nexys A7 has a USB HID host interface for keyboard and mouse which, with the help of clever firmware on a PIC24 microcontroller, presents itself to the FPGA as a PS/2 interface. See the [Nexys A7 Reference Manual](https://digilent.com/reference/programmable-logic/nexys-a7/reference-manual) for more details.

### I2C

The I2C interface can be used to hook up a [Real-Time Clock PMOD](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/) as well as a [Wii Nunchuck Adapter](https://www.reichelt.com/be/en/arduino-8211-wiichuck-nunchuck-adapter-ard-wii-nunchuck-p282673.html?CCOUNTRY=661&LANGUAGE=nl&GROUPID=9020&START=0&OFFSET=16&SID=93757c8e4582e90848068d74dbb71d4a2c938ebd13432dc6b9c96&LANGUAGE=EN&&r=1).

ZipCPU has an I2C core with a Wishbone port: [https://github.com/ZipCPU/wbi2c](https://github.com/ZipCPU/wbi2c).

### Serial Port

ZipCPU comes to the rescue once again with a UART implementation with a Wishbone interface: [https://github.com/ZipCPU/wbuart32](https://github.com/ZipCPU/wbuart32)

Location of the *wbuart32* submodule in the BoxLambda repository: **boxlambda/fpga/wbuart32/**

### Miscellaneous Modules

- **DFX Controller**: The actual loading of a Reconfigurable Module into a Reconfigurable Partition is handled by the DFX Controller. DFX stands for **Dynamic Function Exchange** which is Xilinx-speak for Partial FPGA Reconfiguration.
- **ICAP**: Internal Configuration Access Port. This module gives access to the FPGA configuration functionality built into Xilinx FPGAs. We'll use the ICAP to implement in-system updates of the Full Configuration Bitstream, loaded into the FPGA upon boot-up.
- **Quad SPI Flash**: This is a module provided by Xilinx, giving access to the Flash Memory device attached through a Quad-SPI bus. The non-volatile Flash Memory will hold the Full Configuration Bitstream(s), System Firmware, and non-volatile system configuration parameters such as keyboard type
- **wb_gpio**: A simple GPIO core with a Wishbone interface, for sampling buttons and switches, and driving LEDs. *wb_gpio.v* is included in the *Ibex_WB* submodule.

Git Workflow
------------

All GitHub repositories used by BoxLambda are instantiated in BoxLambda's repository as git submodules. The git submodules are located in the *sub/* directory:

```
sub
├── ibex
├── ibex_wb
├── wbuart32
└── ...
```

Each of the git submodules is a fork of a GitHub project discussed in earlier posts. For example, *boxlambda/projects/ibex/* contains [my ibex fork](https://github.com/epsilon537/ibex), not the [original ibex repository](https://github.com/lowRISC/ibex).

In each of the forked submodules, two branches are relevant:

- **master**: I'm keeping the master branch in sync with the master branch of the repository I forked from. Having this branch makes it easy to pull in updates as well as to submit the occasional pull request to the original project.
- **boxlambda**: On this branch, I'll be making changes for BoxLambda.

In the BoxLambda repository itself, I have the following long-running branches:

- **master**: I will submit releases to this branch. The master branch should always be in good shape.
- **develop**: This is where the work is happening. Things will be in flux here. This branch will not always be in good shape.
- **gh-pages**: This branch holds the BoxLambda Blog files. GitHub Pages are by default on the *gh-pages* branch of a GitHub project.
- **boxlambda-gh-pages-wip**: This branch holds work-in-progress Blog updates. This branch also contains some config file modifs specifically for local previewing, which is why this is a long-running branch, rather than a topic branch. When updates are ready for release, I merge them to *gh-pages*. 

The Build System
----------------

### Bender

The build system is makefile based but relies on Bender for dependency Management:

[https://github.com/pulp-platform/bender](https://github.com/pulp-platform/bender)

Central to Bender is the package manifest *bender.yml*. In the manifest, you specify the HDL sources that make up the package, dependencies, include paths, targets (e.g. synth, sim), and associated *defines*.
A package directory is a directory containing a bender.yml file. When you run bender in that directory, you can ask it to generate a flat list of all the sources from the current package, and the packages it depends on. Optionally, it can generate that list, plus any *defines* associated with a given target, as a Tcl script. This makes integration with Vivado very easy.

![Project View of the Build System](../assets/Project_Build_Diagram.drawio.png){:class="img-responsive"}

*Project View of the Build System*

### Three Layers

The build system has three layers:

1. **The Project Layer (top)**: *Hello World* is an example project. A project is the top layer of the build system. The bender.yml manifest contains the top-level files of an SoC build, the project's *.xdc* constraints file, memory files used by the SoC, and a list of *components* the project depends on. 
2. **The Component Layer (middle)**: Components are the middle layer of the build system. They are the building blocks of an SoC. A component's sources, *defines*, and dependencies are defined in a bender.yml manifest. A component gets its HDL sources from its *rtl/* subdirectory and/or from *sub/*, the submodule layer. I'm considering each Wishbone Bus Master or Slave a component.
3. **The Submodule Layer (bottom)**: Submodules are the bottom layer of the build system. They are the Git Submodules that BoxLambda is referencing, as [discussed previously](https://epsilon537.github.io/boxlambda/git-workflow-and-setup/).

The repository's directory structure reflects the three layers:

```
boxlambda
├── build_sys
├── projects
│   └── hello_world
├── components
│   ├── ibex
│   ├── ibex_wb_common
│   ├── ibex_wb_core
│   ├── wb_gpio
│   └── wbuart32
└── sub
    ├── ibex
    ├── ibex_wb
    └── wbuart32
```

### The Project Build Makefile

A project directory, such as *projects/hello_world/*, contains a Makefile, with the following build targets:

- **dryrun**: Generate a Vivado project, but don't build it.
- **synth**: Generate a Vivado project and synthesize it.
- **impl**: Generate a Vidado project, synthesize it, and implement it.
- **run**: Download the generated bitstream file to the target. Note: The script this build target executes is configured for my WSL-based setup. It may need customization for other setups.
- **clean**: Remove all generated files in the current directory and subdirectories.
- **lint**: Run Verilator lint checking on the project and all of its dependencies.
- **make sim**: Build the project's Verilator test bench.
- **make test**: Build the project's Verilator test bench, then run it in batch mode (non-interactive mode).
  
#### What happens when you run *make synth*

When you run *make synth*, the following happens:
1. Make runs a *bender script* command. 
2. The bender script command processes the current directory's package manifest (*bender.yml*), as well as the package manifests of any dependent components. 
3. The bender script command emits a list of all the HDL sources that make up the project. 
4. Make feeds this file list, along with a *.xdc* constraints file and any *.mem* memory files, into a *vivado.tcl* script. 
5. The vivado.tcl script generates a Vivado project file containing all the HDL sources, constraints, and memory files. 
6. The vivado.tcl script kicks off synthesis and generates timing and utilization reports when synthesis is complete.

When you run *make impl*, the same thing happens, but after completing synthesis, the vivado.tcl script proceeds to kick off implementation and bitstream generation.

![The Build System Files - arrows indicate information flow](../assets/Build_System_Files.drawio.png){:class="img-responsive"}

*The Build System Files - arrows indicate information flow.*

The relevant files are linked below. To avoid repeating identical rules and variables across Makefiles, a *build_sys/common.mk* include file is created which contains all reusable Makefile logic.

- [build_sys/common.mk](https://github.com/epsilon537/boxlambda/blob/79a78e8425d80836294669aaa0efebf6b4cbdb99/build_sys/common.mk)
- [projects/hello_world/Makefile](https://github.com/epsilon537/boxlambda/blob/79a78e8425d80836294669aaa0efebf6b4cbdb99/projects/hello_world/Makefile)
- [projects/hello_world/Bender.yml](https://github.com/epsilon537/boxlambda/blob/79a78e8425d80836294669aaa0efebf6b4cbdb99/projects/hello_world/Bender.yml)
- [build_sys/vivado.tcl](https://github.com/epsilon537/boxlambda/blob/79a78e8425d80836294669aaa0efebf6b4cbdb99/build_sys/vivado.tcl)

### The Component Makefile

A component Makefile works the same way as a project Makefile. Make targets **clean**, **lint** and **synth** are defined, and do the same thing as in a project build. When you run **make synth**, keep in mind that you're running a partial synthesis. The component's input and output ports aren't hooked up to anything. 

![Component View of the Build System](../assets/Component_Build_Diagram.drawio.png){:class="img-responsive"}

*Component View of the Build System*

### The Root Makefile

The Makefile at the root of the repository has the following targets defined:

- **make clean**: Recursively run **make clean** in each component and project directory.
- **make lint**: Recursively run **make lint** in each component and project directory.
- **make synth**: Recursively run **make synth** in each component and project directory.
- **make test**: Recursively builds and runs the Verilator test bench in each project directory. *make test* fails if any of the executed test benches flag a test failure (via a non-zero return code).

### Makefile Variables

Component and Project Makefiles define the following Makefile variables:
- **TOP_DIR**: Relative path to the root directory of the repository.
- **TOP_MODULE**: Identifies the top RTL module of that particular build. This info is passed on to both Verilator and the Vivado synthesizer. Specifying the top module in a design avoids ambiguity and associated build warnings/errors.

Additionally, component Makefiles define the following Makefile variable:
- **OOC**: This is a flag indicating Out-of-Context Synthesis. The value corresponds to the Bender.yml target used to select OOC specific files and defines.


### About Memory Files

Memory files used by an FPGA build are typically generated from software. It would be annoying to have to build the hello world program, to generate a memory file, and then build the FPGA in a separate step. As a rule, a build system should start from sources, not from build artifacts created separately by other build systems. 

To combine the software and FPGA build steps, the build system has a pattern rule for *.mem* memory files. Whenever the build system encounters such a file as a dependency, it goes to that file's directory and runs make there, to make sure that the *.mem* file gets generated.

```
#Pattern rule for memory files: Go to the memory file's directory and run Make there.
%.mem : force
	$(MAKE) -C $(@D)
```

The current mechanism just assumes that the default rule in the recursive make will do the right thing. It's a bit crude, but it's a start.

### Bender Targets

Currently, the build system uses the following Bender targets:

- ***module_name***: set when building a component separately (i.e. running **make synth** in a component directory). For example:

```
  - target: ibex_wb_core
    files:
      - rtl/ibex_wb_core_wrapper.sv    
```

- **vivado**: set when synthesizing using Vivado.
- **verilator**: set when linting using Verilator.
- **memory**: set when retrieving memory files for this component or project.
- **constraints**: set when retrieving *.xdc* constraints files for this component or project.

### Verilator Lint Waivers

Rather than add lint waivers to the source code of git submodules, the waivers are grouped into *.vlt* files that live in the corresponding component's subdirectory. This way we avoid making unnecessary code changes in the git submodules.

For example:
[components/ibex/lint.vlt](https://github.com/epsilon537/boxlambda/blob/60917b7521553e19760868957e6bf05069946a2f/components/ibex/lint.vlt)

Component/Project Directory Layout
----------------------------------
A component or project directory typically contains the following files and subdirectories:

```
<component/project>
├── Makefile
├── Bender.yml: The component/project Bender manifest.
├── lint.vlt: Lint waivers.
├── generated
│   └── <files and directories generated during synthesis or verilation. 
├── src
│   └── <Boxlambda specific HDL sources for given component/project>
└── sim
    └── <C++ test bench code for given component/source>
```

The *generated/* directory is created by the build system. It is not version-controlled.

Test Bench
----------
The focus should be on system-level testing rather than component-level verification. The components themselves have already been verified by their respective owners.

Ideally, the test bench should allow for the following:
- Execute system-level test cases in a reasonable time frame. With system-level test cases, I mean test cases where the DUT is the SoC.
- A short lather-rinse-repeat cycle of making code changes and testing them on a system-level DUT.
- Full signal visibility into the build, to aid test case development as well as debugging.
- *Reasonably* easy automated testing. With the caveat that automated testing is never truly *easy*.

### Verilator

Boxlambda uses Verilator to create test benches. Verilator is a compiler. It compiles, or rather *verilates*, an HDL design into a C++ model. It then picks up any user-provided C++ testbench/wrapper code and compiles the whole thing into an executable, optionally with the ability to generate traces.

C++ is not an ideal language for test case development, but it'll get the job done, and it's a compiled language, so it's *fast*. 

### A simple Test Bench

I created a proof-of-concept test bench for the *Hello World* build. I started from the example code included in the Verilator distribution:

[https://github.com/verilator/verilator/blob/master/examples/make_tracing_c/sim_main.cpp](https://github.com/verilator/verilator/blob/master/examples/make_tracing_c/sim_main.cpp)

I included *UARTSIM*, the UART co-simulation class that ZipCPU provides along with the UART Verilog implementation in the *wbuart32* repository:

[https://github.com/epsilon537/wbuart32/tree/master/bench/cpp](https://github.com/epsilon537/wbuart32/tree/master/bench/cpp)

The test bench does the following:
1. Instantiate the verilated *Hello World* model and the UARTSIM co-simulation object.
2. Optionally, controlled by a command-line option, enable tracing.
3. Run the model for a fixed number of clock cycles.
4. While running the model:
   1. Feed the model's UART output to UARTSIM.
   2. Capture and display the decoded UARTSIM output and the GPIO outputs.
5. Pass/Fail criterium: After running the model for the set number of clock cycles, match the captured UART and GPIO outputs against expected results.

As suggested by ZipCPU in his Verilog tutorial, I use *nCurses* for positional printing inside the terminal windows. This way, I can easily build a display that refreshes, rather than scrolls, whenever the model produces new UART or GPIO data to display.

The test bench source code is located in [*projects/hello_world/sim/sim_main.cpp*](https://github.com/epsilon537/boxlambda/blob/6c3c0b36525cf3f0aef869f9b618759258c2106c/projects/hello_world/sim/sim_main.cpp).

### Are we running in a Simulation?

Software running on Ibex needs to know whether it's running in a simulation or on FPGA, so it can adjust timings such as the LED blink period.
I'm using GPIO1 bits 3:0 for this purpose. In a simulation, I set these bits to *4'bf*. On FPGA I set them to something else.
The *hello.c* test program includes the following check:

```
  //GPIO1 bits3:0 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR)
    uart_printf(&uart0, "This is a simulation.\n");    
  else
    uart_printf(&uart0, "This is not a simulation.\n");
```

### Files and command line options

All files created by Verilator go in the *\<project_dir\>/generated/* subdirectory. The name of the generated executable is **Vmodel**.
As you can see in the *sim_main.cpp* source code above, *Vmodel* accepts a few command line options:

- **Vmodel -t**: Execute with waveform tracing enabled. The program generates a *.fst* trace file in the current directory. *.fst* files can be viewed with **gtkwave**.

![Gtkwave View of Waveform Trace Generated by *Hello World* Verilator Test Bench](../assets/hello_world_gtkwave.jpg){:class="img-responsive"}
*Gtkwave View of Waveform Trace Generated by *Hello World* Verilator Test Bench*

- **Vmodel -i**: Run in interactive mode, vs. the default batch mode. In interactive mode, the program may wait for keypresses. Batch mode is used for non-interactive automated testing.

Prerequisites
-------------

- **Host OS**: Linux or Linux WSL.
- **Vivado ML** Edition V2021.2, Linux version:
  
  [https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2021-1.html](https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2021-1.html)
  
  Make sure you also install your Arty A7 or Nexys A7 board files. Digilent has excellent instructions for installing Vivado and Digilent board files:
  
  [https://digilent.com/reference/vivado/installing-vivado/v2019.2](https://digilent.com/reference/vivado/installing-vivado/v2019.2)

- RISCV Compiler Toolchain **rv32imcb**. This is the cross compiler for building the code that'll run on the Ibex processor. I'm using the **20220210-1** pre-built binaries from *lowRISC*: 
	[https://github.com/lowRISC/lowrisc-toolchains/releases](https://github.com/lowRISC/lowrisc-toolchains/releases)

  Add the toolchain's *bin/* directory to your *PATH*. E.g.:

```
export RISCV_TOOLCHAIN=$HOME/lowrisc-toolchain-gcc-rv32imcb-20220210-1
export PATH=$PATH:$RISCV_TOOLCHAIN/bin
```

- **GNU Make** version 4.2.1: [https://www.gnu.org/software/make/](https://www.gnu.org/software/make/)
  
  Please make sure make is in your *PATH*.
  
- **Bender** 0.25.2: [https://github.com/pulp-platform/bender](https://github.com/pulp-platform/bender)

  Add bender to your *PATH*.

- **Verilator** 4.216: [https://verilator.org/guide/latest/install.html](https://verilator.org/guide/latest/install.html)

  Add verilator to your *PATH*.

- **Ncurses**: ```sudo apt-get install libncurses5-dev libncursesw5-dev libncursesw5```

- **Gtkwave**: [http://gtkwave.sourceforge.net/](http://gtkwave.sourceforge.net/)

- **RISCV OpenOCD**

  Build RISCV OpenOCD from source:
  1. ```git clone https://github.com/riscv/riscv-openocd```
  2. ```cd riscv-openocd```
  3. ```git submodule update --init --recursive```
  4. ```./bootstrap```
  5. ```./configure --prefix=$RISCV --disable-werror --disable-wextra --enable-remote-bitbang --enable-ftdi```
  6. ```make```
  7. ```sudo make install```
  8. Add the install directory (*/usr/local/bin* in my case) to your PATH.  
	 &nbsp;

Test Builds
-----------

Before you try any of the Test Builds below, you need to set up the repository:

   1. Install the [Prerequisites](../documentation/#prerequisites). 
   1. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
   1. Switch to the *hello_dbg* tag: 
```
git checkout hello_dbg
```
   1. Get the submodules: 
```
git submodule update --init --recursive
```

### Hello World on the Arty A7-35T

Project directory **boxlambda/projects/hello_world/** contains a test SoC build consisting of an Ibex_WB core, 64KB internal memory, a wbuart32 core, a timer, and a GPIO module.

To build the *Hello World!* example, go through the following steps:

1. Build the project:
   1. ```cd projects/hello_world```
   2. ```make impl```  
1. Start Vivado and download the generated bitstream to your Arty A7-35T: *projects/hello_world/generated/project.runs/impl_1/ibex_soc.bit*

### Hello World Verilator Build

To try out the Verilator Test Bench for *Hello World*:

1. Build the testbench:
   1. ```cd projects/hello_world```
   2. ```make sim```
1. Execute the testbench:
   1. ```cd generated```
   2. Without tracing (fast): ```./Vmodel -i```
   3. With tracing (slow): ```./Vmodel -t```
1. View the generated traces: ```gtkwave simx.fst```

### Connecting GDB to the Hello_DBG build on Arty A7

1. Build the test project:
```
cd projects/hello_dbg
make impl
```
1. Start Vivado and download the generated bitstream to your Arty A7-35T:  
   *projects/hello_dbg/generated/project.runs/impl_1/ibex_soc.bit*
1. Verify that the *Hello World* test program is running: The four LEDs on the Arty A7 should be blinking simultaneously.
1. If you're running on WSL, check the **When on WSL** note below.
1. Start OpenOCD with the *digilent_arty_a7.cfg* config file: 
```
sudo openocd -f <boxlambda root directory>/openocd/digilent_arty_a7.cfg
Info : clock speed 1000 kHz
Info : JTAG tap: riscv.cpu tap/device found: 0x0362d093 (mfg: 0x049 (Xilinx), part: 0x362d, ver: 0x0)
Info : [riscv.cpu] datacount=2 progbufsize=8
Info : Examined RISC-V core; found 1 harts
Info :  hart 0: XLEN=32, misa=0x40101106
[riscv.cpu] Target successfully examined.
Info : starting gdb server for riscv.cpu on 3333
Info : Listening on port 3333 for gdb connections
Ready for Remote Connections
Info : Listening on port 6666 for tcl connections
Info : Listening on port 4444 for telnet connections
```
1. Launch GDB with hello.elf:	
```
cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
riscv32-unknown-elf-gdb hello.elf
```
1. Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
0x00000c90 in delay_loop_ibex (loops=3125000) at ../../libs/soc/utils.c:12
12              asm volatile(
```

#### When on WSL

If you're running on WSL, you need to make sure that the USB port connected to the Arty A7 is forwarded to WSL. The following article describes how to do this:

[https://docs.microsoft.com/en-us/windows/wsl/connect-usb](https://docs.microsoft.com/en-us/windows/wsl/connect-usb)

On my machine, these are the steps:

1. From a Windows Command Shell:
	
	```
	C:\Users\ruben>usbipd wsl list
	BUSID  VID:PID    DEVICE                                                        STATE
	1-2    0403:6010  USB Serial Converter A, USB Serial Converter B                Not attached
	1-3    0461:4d15  USB Input Device                                              Not attached
	1-7    13d3:5666  USB2.0 HD UVC WebCam                                          Not attached
	1-14   8087:0aaa  Intel(R) Wireless Bluetooth(R)                                Not attached

	C:\Users\ruben>usbipd wsl attach --busid 1-2
	```

1. From a Linux shell on WSL:

	```
	epsilon@LAPTOP-BQA82C62:~$ lsusb
	Bus 002 Device 001: ID 1d6b:0003 Linux Foundation 3.0 root hub
	Bus 001 Device 002: ID 0403:6010 Future Technology Devices International, Ltd FT2232C/D/H Dual UART/FIFO IC
	Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
	```

### Connecting GDB to the Hello_DBG build on Verilator

![OpenOCD JTAG Debug Session on Verilator](../assets/Verilator_Debug_Session.jpg){:class="img-responsive"}
*OpenOCD JTAG Debug Session on Verilator*

1. Build the test project:

	```
	cd projects/hello_dbg
	make sim
	```
1. Launch the Verilator model:

	```
	cd generated
	./Vmodel
	```
1. Start OpenOCD with the *verilator_riscv_dbg.cfg* config file:

	```
	openocd -f <boxlambda root directory>/openocd/verilator_riscv_dbg.cfg
	Open On-Chip Debugger 0.11.0+dev-02372-g52177592f (2022-08-10-14:11)
	Licensed under GNU GPL v2
	For bug reports, read
			http://openocd.org/doc/doxygen/bugs.html
	TAP: riscv.cpu

	[riscv.cpu] Target successfully examined.
	Ready for Remote Connections on port 3333.
	```
1. Launch GDB with hello.elf:	

	```
	cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
	riscv32-unknown-elf-gdb hello.elf
	```
1. Connect GDB to the target. From the GDB shell:

	```
	(gdb) target remote localhost:3333
	Remote debugging using localhost:3333
	0x000005fc in uart_tx_ready (module=<optimized out>) at ../../libs/soc/uart.c:31
	31              return module->registers[UART_REG_FIFO] & 0x00010000;
	```
