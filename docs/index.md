About BoxLambda
===============

![BoxLambda Architecture Block Diagram](assets/Arch_Diagram_NoDFX.png)

**BoxLambda** is a project creating a retro-style FPGA-based microcomputer from open-source components. The microcomputer serves as a platform for software and RTL experimentation.

BoxLambda is a software-hardware cross-over project. The project provides room for experimentation both on the FPGA RTL side and on the software side.

Key Goals
---------
- Create a sandbox for experimenting with software and FPGA gateware:
    - It should be **easy** to jump in and do something: create, hack, tinker.
        - **Simple Enough for One Person**: A motivated person can develop a good understanding of the entire system, software and hardware.
        - **Deterministic Behavior**: By design, the duration of operations such as internal memory or register access must be predictable.
        - **Self-Contained Run-Time Environment** supporting high-level interactive and low-level systems programming.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components.
- Target Hardware is Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start).
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

Status (Dec. 2024)
------------------

- **Infrastructure** (build system etc.): Complete, with room for simplification.
- **Gateware**: Complete as pictured in Block Diagram. Of course, by definition there's always room for additional/alternate components. 
- **Software**: Early stage development.

GitHub
------
**Source Code**: [https://github.com/epsilon537/boxlambda/](https://github.com/epsilon537/boxlambda/)

**Project Blog**: [https://epsilon537.github.io/boxlambda/](https://epsilon537.github.io/boxlambda/)

