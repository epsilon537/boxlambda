---
layout: post
title: 'BoxLambda OS Software Architecture, First Draft.'
comments: true
mathjax: yes
---

About six months ago, to my surprise, I completed the BoxLambda SoC's gateware.
Since then, I have explored various OS projects, programming languages, tools and
ideas for BoxLambda's software environment. It's time to commit to a
software architecture for BoxLambda.

# Context

Here's a TLDR, for those who haven't read all 33 previous posts:
BoxLambda is a hardware-software cross-over project creating a homebrew,
retro-style FPGA-based microcomputer. The goal is to create a sandbox
environment for experimenting with software and FPGA gateware.

The physical setup looks like this:

[![The physical
setup.](../assets/physical_setup.png)](../assets/physical_setup.png)

*The physical setup.*

BoxLambda's current features:

- Target FPGA: Arty-A7-100T.
- Ibex RISC-V core with machine timer and hardware interrupt support.
- Harvard architecture-based interconnect.
- Low-latency register and memory access across the SoC.
- Predictable instruction cycle counts.
- DFX Partial FPGA Reconfiguration support.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites,
128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- Picolibc-based standard C environment.
- Test application suite covering all SoC components, running on both FPGA and
Verilator.
- A Linux CMake and Bender-based Software and Gateware build system.

This is the gateware's block diagram:

![BoxLambda Block Diagram.](../assets/Arch_Diagram_dual_bus_DFX.png)

Project documentation: [https://boxlambda.readthedocs.io/en/latest/](https://boxlambda.readthedocs.io/en/latest/)

The Repo: [https://github.com/epsilon537/boxlambda](https://github.com/epsilon537/boxlambda)
<br/><br/>

# OS Requirements

Here are the original requirements relevant for the software side of BoxLambda
(see [first post](https://epsilon537.github.io/boxlambda/introducing-boxlambda/#key-goals)):

- **Simplicity**: It must be easy to jump in and do something: create, hack,
tinker.
- **Grokability**: It must be doable for a single person to develop a good
understanding of the entire system, software and hardware.
- **Deterministic Behavior**: By design, it must be clear how long an operation
  an operation is going to take, be it one instruction or a complete function.
- **Single User/Single Tasking OS** booting to a console shell.

(There are some additional, practical requirements for peripherals, sound, and
graphics, but those are less relevant for the current discussion)

That was three years ago. Meanwhile, I've had time to think about them and my
expectations for the operating system. I would like to introduce the following
additional requirements:

- **Self-Hosting**: The system must be self-contained and self-hosted, i.e. not
depend on a host PC for software development.
- **System Programming**: The programmer must have full access to the system,
i.e. not being restricted to a sandbox/VM environment.
- **REPL**: The programming environment must support interactive programming
using a REPL.
- **Automated Testing**: All features must support automatic testing.

Elaborating on the above:
- The OS must support assembly-language level programming. Think of programming
an Interrupt Service Routine or bit-banging a timing-critical bus, for example.
- I don't want to do all system-level programming in assembly language. I'm
going to need a **Compiled Language**. The compiler must be self-hosted.
- The OS must include low-level tools such as a disassembler, memory editor, and
debugging facilities.
- The OS must support a text editor.

Also worth noting:
- The *Automatic Testing* requirement may result in gateware changes supporting
testability, e.g. a VGA capture mechanism.
- A purely interpreted software environment does not meet the requirements (it
is not suitable for *System Programming* and does not easily provide
*Deterministic Behavior*).
- Even though the system must not *depend* on a host PC, a USB connection to a PC
will still be useful for:
    - File transfers to or from a PC.
    - JTAG debugging of system software issues.
    - Flashing new firmware and bitstream images.
- I'm going to use open-source third-party components wherever I see fit, just
like I did on the Gateware side of the project. In practice, this means that
parts of the OS will consist of cross-compiled C/C++ code. An *FFI* (*Foreign
Function Interface*) between the cross-compiled code and the self-hosted code
will be needed.
<br/><br/>

# Forth

[![The Forth Logo.](../assets/forth_logo.png)](../assets/forth_logo.png)

Forth enthusiasts saw this coming from a mile away: These requirements have
*Forth* written all over them.

To be honest, when I started this project, I did *not* know about Forth, other
than that is was a weird stack-based language. When I finally learned about it
while looking for ideas for this project, I got excited, of course. Forth is
a unique, minimalistic, extremely powerful programming language. It features a
REPL, an interpreter doing double duty as a compiler (a *competer*? *interpiler*?),
an assembler, editor, and metaprogramming capabilities you wouldn't believe.

```
: IF IMMEDIATE ' 0BRANCH , HERE @ 0 , ;

: THEN IMMEDIATE DUP HERE @ SWAP - SWAP ! ;
```
*If..then defined in Forth.*

To get a sense of what a unique language Forth is, check out this article. It's
long but entertaining:

[https://ratfactor.com/forth/the_programming_language_that_writes_itself.html](https://ratfactor.com/forth/the_programming_language_that_writes_itself.html)

Forth just begs to be explored on an experimental homebrew computer such as
BoxLambda. Learning Forth is *not* a walk in the park. It requires a different
way of thinking. At my current level, an average line of low-level Forth takes
almost as long to unpack as a regular expression of the same length. But I
enjoy the challenge! It feels a bit like diving into assembly code on the
Commodore 64 for the first time.
<br/><br/>

# Explorations

[![Explorations.](../assets/explorations.png)](../assets/explorations.png)

I spent several ~~weeks~~ months ~~messing around with~~ evaluating a bunch of Forths and other projects that might be of
interest:

- [Project Oberon](https://www.projectoberon.net): A [delightfully insane
system](https://ignorethecode.net/blog/2009/04/22/oberon/) featuring desktop UI,
programming language, and compiler with a very modest footprint. Full of
interesting alternative OS and UI ideas.
- [DuskOS](https://duskos.org): an insanely smart Forth-based OS including a C
compiler and an Oberon port. I could just port Dusk to BoxLambda and say that
I'm done. It ticks all boxes on my requirements list.
- [ZeptoForth](https://github.com/tabemann/zeptoforth): a modern, feature-rich
and well-documented Forth for ARM-based CPUs. I considered porting it to RISC-V.
- [Mecrisp Quintus Forth and Mecrisp Cube](https://mecrisp.sourceforge.net): A
well-written Forth for RISC-V based systems. An excellent code base to really
*grok* Forth and easy to port to BoxLambda.
- [TCC Tiny C Compiler](https://bellard.org/tcc/): a C compiler that can
conceivably be ported to a constrained platform such as BoxLambda.
- [Lua](https://www.lua.org) and [eLua](https://eluaproject.net). Lua is a
lightweight, byte-code compiled, embeddable scripting language, easy to port, and
with a well-designed C-FFI. I considered Lua as a higher-level scripting
language to complement Forth.
- [uLisp](http://www.ulisp.com): A well-documented and easy to port
implementation of Lisp for MCUs. ULisp supports RISC-V assembly language
functions and has an (experimental?) [Lisp
compiler](http://www.ulisp.com/show?4Y20). Could be an interesting complement to
Forth.
- [viless](https://github.com/brentr/viless): A bare-metal version of *vi*, a
candidate text editor for BoxLambda.
[Kilo](https://viewsourcecode.org/snaptoken/kilo/) is another option.

DuskOS is an amazing project. It would be great to be part of it and to port
that system to BoxLambda. A few things are holding me back, however. The OS and
language is very advanced and not easy to master. It feels a bit like jumping on
a powerful 800cc bike while I'm not used to the 125cc starter bike yet. Also, in
Dusk, most interesting decisions have already been made. Dusk OS is more or less
complete. I'm looking for an OS development project, not just an OS porting
project.

Maybe one day I'll port Dusk, but right now, I prefer to go my own way. It
would be a bit strange to spend all this time carefully putting together this
computer and then just slap in one fell swoop an entire OS on top of it, and be
done with it.

I'll be using Matthias Koch's Mecrisp Quintus Forth as a starting point. Mecrisp leans a bit
more towards traditional Forth, is easier to pick up, and leaves more headroom
for growing into a custom-designed OS.
<br/><br/>

# The OS Architecture

Here's a block diagram of the OS architecture I'm proposing:

[![BoxLambda OS
Architecture.](../assets/BoxLambda_OS_Architecture.png)](../assets/BoxLambda_OS_Architecture.png)

*BoxLambda OS Architecture Block Diagram.*

The two main subsystems are the **BoxLambda Kernel** (**BoxKern**) and the
**Mecrisp Forth Environment**.

The BoxKern image is stored in flash memory. The Bootloader loads the kernel
into EMEM, then transfers control to it. The kernel's *CRT0* startup code then
further unpacks specific sections into EMEM and IMEM. (See
[here](https://boxlambda.readthedocs.io/en/latest/sw_comp_bootloader/) for a
more detailed description of the boot sequence).

The Mercrisp Forth Environment lives as Forth source code on the SD card
filesystem.

## The BoxKern

The BoxLambda Kernel is subdivided into two cores:

- The **Mecrisp Forth Core**, written in RISC-V assembly language.
- The **C Core** containing C/C++ drivers, libraries and C runtime.

The BoxKern is mostly about leveraging existing non-Forth code. Most of the
BoxKern components already exist in BoxLambda's code base, the exceptions being:
- **The Foreign Function Interface (FFI)**: This is the mechanism to be used to
call Forth from C code and C from Forth code. BoxKern drivers and libraries
export their functionality to the Forth environments using the FFI.
- **The Console Driver**: In the current BoxLambda code base, Picolibc's *stdin*
and *stdout* are associated with the serial port. This I/O method has to be
replaced by a Console Driver which forwards output to and takes input from the
Forth-side REPL.

### The Mecrisp Forth Core

The Mecrisp Forth Core is written in RISC-V assembly language. It contains Forth
start-up code and the definitions of key Forth words, such as
interpreter/compiler words, that make up the core of the Forth system. After
booting the Mecrisp Forth Core, you have a small, operational Forth system.

I'm adding Filesystem Access (*FS Access*) words to the set of original Mecrisp
core words so that after booting the core, the remaining Forth words can be
loaded as Forth *.fs* source code files from the SD card filesystem. The
filesystem access words are based on those found in Peter Schmid's [Mecrisp
Cube](https://github.com/spyren/Mecrisp-Cube/blob/master/sdcard/man/FileSystem.md)
project.

## The Mecrisp Forth Environment

[![
Flamingo.](../assets/flamingo.png)](../assets/flamingo.png)
*Mecrisp Forth's Welcome message - ASCII art from Adreas Freise.*

Most *new* code will be written in Forth or in assembly language in the Forth
environment. Not everything is in the Forth environment is new, still-
to-be-developed software, however. Some modules, such as the RISC-V assembler
and disassembler are part of the Mecrisp Forth Quintus distribution.

### The Canvas REPL / Editor

Instead of a line-oriented prompt, I would like to provide a REPL that allows
you to move freely all over the screen, as is the case on the Commodore 64, for
instance. The **Canvas REPL** can perform double duty as a text editor.

[![The Canvas REPL.](../assets/canvas_repl.png)](../assets/canvas_repl.png)

*The Canvas REPL.*

Note that the REPL takes its input from the USB HID keyboards and sends it
output to the attached VGA display. There is no serial port terminal.

### The BoxKern Proxies

The *BoxKern Proxies* are Forth-side proxies of BoxKern drivers and libraries. The
proxies access the BoxKern drivers/libs through the FFI.

## The Extras

The greyed-out parts in the block diagram are components that are not essential
to create a working system. These components are likely added later (or
possibly, never). This is just a crude binary partitioning of what really
is an [iterative spiraling
approach](https://epsilon537.github.io/boxlambda/hello-world/). I start from
something small but working (e.g., the Mecrisp Forth core running in a serial port
terminal) and keep growing it. The greyed-out parts are among the parts to be
added last.

### Binary Executables and ELF Loader

Two such extra components are the *Binary Executables* and *ELF
Loader*. I would like to have a way to extend the non-Forth part of the system
without making the extension part of the kernel monolith. Lua or uLisp could be
added to the system in such a way, for example. These executables can
communicate with the rest of the system through the FFI.

# The Display

The 640x480 display is organized into two layers. The top layer is an 80x60
character grid displaying the Canvas REPL. The bottom layer is a 2-bit-per-pixel
640x480 bitmap. The *GFX Primitives* module will render to this layer by
default.

# Choosing is Losing

I spent an inordinate amount of time evaluating projects and programming
languages for BoxLambda. It's just too much fun. I have prototypes on BoxLambda
of *uLisp*, *Lua*, *Forth*, and *vi*. I wrote a RISC-V disassembler for DuskOS,
and I've been diving into Project Oberon, ELF loaders, and small-footprint C
compilers.

The hard part is deciding it's time to stop. You don't get to define
the OS for a homebrew computer every day, and you never know if you've overlooked
some project or concept that's even cooler and more perfect for your own project
than what you have already discovered.

What would you choose?

