---
layout: page
title: About BoxLambda
permalink: /about/
---

![Draft Architecture Block Diagram](../assets/Arch_Diagram_DFX.png)

**BoxLambda** is a project that creates a retro-style FPGA-based microcomputer from open-source components. The microcomputer serves as a platform for both software and RTL experimentation.

BoxLambda is a software-hardware crossover project, providing opportunities for experimentation on both the FPGA RTL side and the software side.

# Key Goals

- Create a sandbox for experimenting with software and FPGA gateware:
    - It should be **easy** to jump in and do something: create, hack, tinker.
        - **Simple Enough for One Person**: A motivated individual can develop a solid understanding of the entire system, including both software and hardware.
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

# Status (Dec. 2024)

- **Infrastructure** (build system, etc.): Complete, with room for simplification.
- **Gateware**: Complete as pictured in the Block Diagram. Of course, there’s always room for additional or alternate components.
- **Software**: Early stage development.

# GitHub

- **Source Code**: [https://github.com/epsilon537/boxlambda/](https://github.com/epsilon537/boxlambda/)
- **Project Blog**: [https://epsilon537.github.io/boxlambda/](https://epsilon537.github.io/boxlambda/)
- **Project Documentation**: [https://boxlambda.readthedocs.io/en/latest/](https://boxlambda.readthedocs.io/en/latest/)
