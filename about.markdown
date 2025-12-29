---
layout: page
title: About BoxLambda
permalink: /about/
---

BoxLambda is a hardware-software crossover project creating a homebrew,
retro-style FPGA-based microcomputer. The goal is to create a sandbox
environment for experimenting with software and FPGA gateware.

# The Physical Setup

[![The physical
setup.](../assets/physical_setup.png)](../assets/physical_setup.png)

*The physical setup.*

# Current Features

- Target FPGA: Arty-A7-100T.
- Ibex RISC-V core with machine timer and hardware interrupt support.
- Harvard architecture-based interconnect.
- Low-latency register and memory access across the SoC.
- Predictable instruction cycle counts.
- DFX Partial FPGA Reconfiguration support.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers, tile or bitmap mode, 2 banks of 64 sprites,
128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- Picolibc-based standard C environment.
- Test application suite covering all SoC components, running on both FPGA and
Verilator.
- Mecrisp Forth core + C FFI.

# Gateware Block Diagram

![BoxLambda Gateware Block Diagram.](../assets/Arch_Diagram_dual_bus_DFX.png)

# Software Block Diagram

[![BoxLambda OS
Architecture.](../assets/BoxLambda_OS_Architecture.png)](../assets/BoxLambda_OS_Architecture.png)

*BoxLambda OS Architecture Block Diagram.*

# Key Goals

- Create a sandbox for experimenting with software and FPGA gateware:
    - It should be **easy** to jump in and do something: create, hack, tinker.
        - **Simple Enough for One Person**: A motivated individual can develop a solid understanding of the entire system, including software and hardware.
        - **Deterministic Behavior**: The duration of operations such as internal memory or register access must be predictable by design.
        - **Self-Contained Run-Time Environment** supporting high-level interactive and low-level systems programming.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components. Optionally, support Partial FPGA Reconfiguration.

- Target Hardware: Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start).

- The computer supports the following peripherals:
    - USB HID Keyboard
    - USB HID Mouse (optional)
    - USB HID Joystick (optional)
    - Real-Time Clock and Calendar (optional)
    - Serial port
    - SD card storage
    - Flash Memory storage
    - VGA Display
    - Audio output

- Sound and graphics support retro-style 2D demos and gameplay.

# Status (December 2025)

- **Infrastructure** (build system, etc.): Complete.
- **Gateware**: Complete and meeting requirements.
- **Software**: Early stage development. OS architecture defined. BoxLambda C Core partially completed. Mecrisp Forth Core partially completed. Mecrisp Forth Environment not started yet.

# Links

- **Source Code**: [https://github.com/epsilon537/boxlambda/](https://github.com/epsilon537/boxlambda/)
- **Project Blog**: [https://epsilon537.github.io/boxlambda/](https://epsilon537.github.io/boxlambda/)
- **Project Documentation**: [https://boxlambda.readthedocs.io/en/latest/](https://boxlambda.readthedocs.io/en/latest/)
