Requirement Analysis
====================
Simplicity
----------

Simplicity will be a strong guideline when making design choices. It is hard to make something simple. The Simplicity requirement will make system design harder, not easier. For a case in point, see below.

Deterministic Behavior
----------------------

Designing a deterministic system is more complex than designing a system that allows some slack in the completion of operations. However, once such a system is in place, it becomes much easier to reason about it and design applications on top of it, especially applications with real-time requirements.
For instance, it would be pretty cool if the system is designed so that racing-the-beam becomes possible, i.e. time actions within an application's main loop so that they take place on a specific screen scan line and a specific column on that scan line. Think Commodore 64 split raster bars and sprite multiplexing.

Deterministic behavior must be guaranteed only when required by the application. Less deterministic operations are perfectly acceptable when the application does not require full deterministic behavior. E.g. a deterministic application runs from Block RAM with known, fixed memory access latency, while a non-deterministic application may run from bursty external memory.

Self-Contained
--------------

The BoxLambda run-time environment must support high-level interactive and low-level systems programming.
This might be a boot-to-Lua environment, with hooks into a native compiler/assembler toolchain, or a Lisp interpreter supporting inline RISCV assembly, or maybe a Forth based environment. 
Most retro machines boot to BASIC. I don't want to be *that* retro.

Discoverable, Modular Architecture
----------------------------------

BoxLambda consists of a reference configuration. Components can be added, removed, or substituted in this reference configuration.
Software applications must be able to discover and identify components.
Software must also be allowed to probe the entire address space without causing system lock-ups. All bus cycles must be properly terminated.

Optional: Partial FPGA Reconfiguration
--------------------------------------

It would be very cool if a gateware component could be incrementally loaded into the FPGA, using Xilinx's *DFX* (Dynamic Function eXchange) feature. This would allow applications to be packaged along with specific hardware components (e.g. accelerators or peripherals) on which they depend.

I'm considering this feature a stretch goal for the project.

Target Hardware and Peripherals
-------------------------------

The target hardware is Digilent's [Arty A7 100T](https://digilent.com/reference/programmable-logic/arty-a7/start).

The following PMODs are used for peripherals:

- [Pmod MicroSD: microSD Card Slot](https://digilent.com/shop/pmod-microsd-microsd-card-slot/)
- [Pmod USB: keyboard/mouse/gamepad port](https://machdyne.com/product/usb-host-dual-socket-pmod/)
- [Pmod AMP2: Audio Amplifier](https://digilent.com/shop/pmod-amp2-audio-amplifier/)
- [Pmod VGA: Video Graphics Array](https://digilent.com/shop/pmod-vga-video-graphics-array/)
- [Pmod RTCC: Real-time Clock/Calendar](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/)
