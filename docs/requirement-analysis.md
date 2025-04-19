---
hide:
  - toc
---

# Requirement Analysis

## Simplicity

Simplicity will be a key factor when making design choices. It’s hard to make something simple, and the simplicity requirement may actually make system design harder, not easier. For example, see below.

## Deterministic Behavior

Designing a deterministic system is more complex than designing a system that allows some flexibility in completing operations. However, once such a system is in place, it becomes much easier to reason about it and design applications on top of it—especially applications with real-time requirements.

For example, it would be cool if the system is designed so that "racing the beam" becomes possible. This would mean timing actions within an application's main loop so that they occur on a specific screen scan line and column. Think of Commodore 64 split raster bars and sprite multiplexing.

Deterministic behavior must be guaranteed only when required by the application. Less deterministic operations are perfectly acceptable when the application doesn’t need full determinism. For example, a deterministic application might run from Block RAM with known, low, fixed memory access latency, while a non-deterministic application could run from bursty external memory.

Deterministic behavior als implies that instruction cycle counts are known and fixed.

## Self-Contained

The BoxLambda runtime environment must support both high-level interactive programming and low-level systems programming. This might involve a boot-to-Lua environment with hooks into a native compiler/assembler toolchain, a Lisp interpreter supporting inline RISCV assembly, or perhaps a Forth-based environment.

Most retro machines boot to BASIC—I don’t want to go *that* retro.

## Discoverable, Modular Architecture

BoxLambda consists of a reference configuration. Components can be added, removed, or substituted within this configuration. Software applications must be able to discover and identify components.

Software should also be able to probe the entire address space without causing system lock-ups. All bus cycles must be properly terminated.

## Optional: Partial FPGA Reconfiguration

It would be great if a gateware component could be incrementally loaded into the FPGA using Xilinx’s *DFX* (Dynamic Function eXchange) feature. This would allow applications to be packaged along with specific hardware components (such as accelerators or peripherals) they depend on.

I'm considering this feature as a stretch goal for the project.

## Target Hardware and Peripherals

The target hardware is Digilent's [Arty A7 100T](https://digilent.com/reference/programmable-logic/arty-a7/start).

The following PMODs are used for peripherals:

- [Pmod MicroSD: microSD Card Slot](https://digilent.com/shop/pmod-microsd-microsd-card-slot/)
- [Pmod USB: keyboard/mouse/gamepad port](https://machdyne.com/product/usb-host-dual-socket-pmod/)
- [Pmod AMP2: Audio Amplifier](https://digilent.com/shop/pmod-amp2-audio-amplifier/)
- [Pmod VGA: Video Graphics Array](https://digilent.com/shop/pmod-vga-video-graphics-array/)
- [Pmod RTCC: Real-time Clock/Calendar](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/)

