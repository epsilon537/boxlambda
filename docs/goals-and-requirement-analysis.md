Goals and Requirement Analysis
==============================
Goals
-----

- Create a sandbox for experimenting with software and (FPGA) hardware.
    - **Simplicity**: It should be easy to jump in and do something: create, hack, tinker.
        - It should be doable for a single person to develop a good understanding of the entire system, software and hardware.
        - **Deterministic Behavior**: By design, it should be clear how long an operation, be it an instruction or a DMA transfer, is going to take.
        - **Single User/Single Tasking OS** booting to a console shell.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components.
        - Support for **partial FPGA reconfiguration**.
- Target Hardware is Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start).
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

Simplicity will be a strong guideline when making design choices. It is hard to make something simple. The Simplicity requirement will make system design harder, not easier. For a case in point, see below.

#### Deterministic Behavior

Designing a deterministic system is more complex than designing a system that allows some slack in the completion of operations. However, once such a system is in place, it becomes much easier to reason about it and design applications on top of it, especially applications with real-time requirements.
For instance, it would be pretty cool if the system is designed so that racing-the-beam becomes possible, i.e. time actions within an application's main loop so that they take place on a specific screen scan line and a specific column on that scan line. Think Commodore 64 split raster bars and sprite multiplexing.

Note that deterministic behavior must be guaranteed only when required by the application. Less deterministic operations are perfectly acceptable when the application does not require full deterministic behavior. E.g. a deterministic application runs from Block RAM with known, fixed memory access latency, while a non-deterministic application may run from bursty external memory.

#### Single-User / Single-Tasking OS

We won't be running Linux or any other multitasking OS for that matter. The platform will only run one application at a time and that application will be fully in charge of the entire system.

A single-user / single-tasking OS will provide the following services:

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

I don't want to be pinned down to, or give preference to, any particular interpreted language, so we're not going to Boot-to-basic.
We're not going for full-retro boot-to-basic.

I would like to allow open support for multiple interpreted languages by letting the application image indicate in which language it's written, e.g. by specifying on the first line the path to the interpreter to use, as commonly used in Linux scripting: *#!/usr/bin/lua, #!/usr/bin/ulisp, ...*

It should also be possible to directly execute binary images of course.

### Modular Architecture

I imagine a reference configuration to which hardware components can be added or from which components can be removed.
Applications should be able to discover, with the help of the OS, whether a certain component is present or not.

#### Partial FPGA Reconfiguration

It would be very cool if a hardware component could be incrementally loaded into the FPGA, using Xilinx's *DFX* (Dynamic Function eXchange) feature. This would allow applications to be packaged along with specific hardware components (e.g. accelerators or peripherals) on which they depend.

I'm considering this feature a stretch goal for the project.

### Target Hardware and Peripherals

I'm currently maintaining a **Big** and a **Little** Configuration. The Big Configuration has more internal memory and more space for optional components than the *Little* Configuration. The *Big* configuration runs on an **Arty A7 100T**. The *Little* Configuration uses an **Arty A7 35T**.

The following PMODs are used for peripherals:

- [Pmod MicroSD: microSD Card Slot](https://digilent.com/shop/pmod-microsd-microsd-card-slot/)
- [Pmod USB: keyboard/mouse/gamepad port](https://machdyne.com/product/usb-host-dual-socket-pmod/)
- [Pmod AMP2: Audio Amplifier](https://digilent.com/shop/pmod-amp2-audio-amplifier/)
- [Pmod VGA: Video Graphics Array](https://digilent.com/shop/pmod-vga-video-graphics-array/)
- [Pmod RTCC: Real-time Clock/Calendar](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/)
