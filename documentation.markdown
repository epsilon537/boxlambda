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
- **Console**: The physical terminal consisting of a screen, a keyboard and optionally a mouse. Console I/O means input/output from/to these physically attached devices.

- **Hacker/Hacking**: See [http://www.paulgraham.com/gba.html](http://www.paulgraham.com/gba.html)

- **(Software) Image**: Snapshot of computer memory contents stored as a file.

- **SoC**: System-on-a-Chip. A System-on-a-Chip is an integrated circuit that integrates all or most components of a computer or other electronic system.

- **ISA**: Instruction Set Architecture. The Instruction Set Architecture is the part of the processor that is visible to the programmer. 

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

One consequence of the *Deterministic Behavior* requirement is that bus arbitration should be done using fixed time slots to be able to guarantee fixed timing, latency and bandwidth to each bus master.

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

It would be very cool if a hardware component can be incrementally loaded into the FPGA, using Xilinx' *DFX* (Dynamic Function eXchange) feature. This would allow applications to be packaged along with specific hardware components (e.g. accelerators or peripherals) on which they depend.

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

Key Components
--------------

We're building a System-on-a-Chip (*System-on-an-FPGA*?). This section identifies the Key Components of the BoxLambda SoC.

### The Bus

The Bus, or interconnect, is the fabric stitching together the SoC internal components. For this project, the two most relevant SoC internal bus specifications are [ARM's AXI bus](https://developer.arm.com/documentation/ihi0022/latest) and the Open-Source [Wishbone bus](https://wishbone-interconnect.readthedocs.io/en/latest/).

AXI is very powerful, very popular, and very complex. It scales up well to very big SoCs. However, I don't think it scales down very well to simple SoCs, such as BoxLambda, where low latency and low complexity are more important than high bandwidth and scalability. Hence, for this project I'm electing to go with **Wishbone**. 

We'll be using the [Wishbone B4 specification](https://github.com/fossi-foundation/wishbone/blob/master/documents/spec/wbspec_b4.pdf).

Sticking to a well-defined internal bus specification certainly helps to meet the Modular Architecture Requirement. Whether we can also accommodate Partial FPGA Reconfiguration using a Wishbone Interconnect remains to be seen.

### The Processor

#### Processor Word Size

Typical processor word sizes are 8-bit, 16-bit, 32-bit and 64-bit. Which word size is the best fit for Boxlambda?

- **8-bit**: A *good* word size. 
  - *Pros*: 
	- An 8-bit word (i.e. a *byte*) is a good natural fit for a pixel value, an ASCII character code, or small integer values. 
	- 8-bit processors, their programs and their data are very compact. 
	- 8-bit processors side-step some of the alignment issues seen with larger word sizes.
  - *Cons*: 
	- An 8-bit word is too small to conveniently hold the values you need in a typical program - think calculations and table indices. 
	- Toolchain support for higher level languages is limited.
- **16-bit**: A clumsy compromise between 8-bit and 32-bits. Made sense when 32-bit processors were not readily available yet. Now, not so much.
- **32-bit**: Another *good* word size.
  - *Pros*: 32-bit words can hold most real-world numbers and cover a huge address space. 32-bit machines generally have good toolchain support.
  - *Cons*: Much bigger than its 8-bit counterpart, in terms of FPGA real-estate, program size as well as data size.
- **64-bit**: A big and clunky word size, way too big to handle conveniently, intended for specialized use cases that don't fit this project.

I've decided to go for a **32-bit processor**. A 32-bit processor (and associated on-chip memory) will take a bigger chunk out of our FPGA real-estate, but I think it's worth it. I really like the convenience of 32-bit registers, and a 32-bit processor may come with a regular GCC toolchain.

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

There are a lot of RISC-V implementations to choose from. The [**Ibex**](https://github.com/lowRISC/ibex) project seems like a good choice:

- 32-bit RISC-V.
- High quality, well-documented implementation.
- SystemVerilog based. My prefered HDL.
- Supports a *small* two-stage pipeline parameterization.
- Very active project.

### The Memory Controller

SDRAM memory access is pretty complicated. Memory access requests get queued in the memory controller, scheduled, and turned into a sequence of commands that vary in execution time depending on the previous memory locations that were recently accessed. 

There exists a class of memory controllers, called **Static Memory Controllers**, that absorb these complexities and by design create a fixed schedule for a fixed use case, resulting in very predictable behavior. Static Memory Controllers are far off the beaten path however. **Dynamic Memory Controllers** are more common. Dynamic Memory Controllers can handle are variety of use cases with good performance *on average*. Unfortunately, they sacrifice predictability to achieve this flexibility.

Ideally, we would use an accessible, well documented, open-source, static memory controller design. Unfortunately, I can't find one. Rolling our own is not an option either. Doing so would require so much specific know-how, it would kill this project. Pragmatically, our best option is to use Xilinx's [**Memory Interface Generator** (MIG)](https://docs.xilinx.com/v/u/1.0-English/ug586_7Series_MIS) with the Arty A7 (or Nexys A7) parameters as [published by Diligent](https://github.com/Digilent/Arty/tree/master/Resources/Arty_MIG_DDR3?_ga=2.230252508.1917430070.1649263055-373952187.1630942771).

The Xilinx memory controller falls squarely into the Dynamic Memory Controller class. How do we fit this into a platform that requires deterministic behavior? I think the best approach is to use a DMA engine to transfer data between SDRAM and on-chip memory. Fixed memory access latency to on-chip memory (from any bus master that requires it) can be guaranteed using an arbiter. We'll revisit this topic when we're discussing Boxlambda's architecture.

### The Graphics Subsystem

### Sound

### Storage

### Keyboard and Mouse

### Serial Port

### Wii Nunchuck
