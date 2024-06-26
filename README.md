**BoxLambda** is an open-source project with the goal of creating a retro-style FPGA-based microcomputer. The microcomputer serves as a platform for software and RTL experimentation.

BoxLambda is a software-hardware cross-over project. The plan is to provide room for experimentation both on the FPGA RTL side and on the software side.

# Key Goals

- Create a sandbox for experimenting with software and (FPGA) HW.    
    - **Simplicity**: It should be easy to jump in and do something: create, hack, tinker.
        - It should be doable for a single person to develop a good understanding of the entire system, software and hardware.
        - **Deterministic Behavior**: By design, it should be clear how long an operation, be it an instruction or a DMA transfer, is going to take.
        - **Single User/Single Tasking OS** booting to a console shell.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components.
        - Support for partial FPGA reconfiguration.
- Target Hardware is Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start) 
- The computer should support the following peripherals:
  - Keyboard
  - Mouse (optional)
  - Joystick (optional)
  - Serial port
  - SD card storage
  - VGA Display
  - Audio output
  
- Sound and graphics should be sufficient to support retro-style 2D gameplay.

# Project Blog 

[https://epsilon537.github.io/boxlambda/](https://epsilon537.github.io/boxlambda/).

# Project Documentation 

[https://boxlambda.readthedocs.io/en/latest/](https://boxlambda.readthedocs.io/en/latest/).

# Setup and Build Instructions

[https://boxlambda.readthedocs.io/en/latest/installation-and-test-builds/](https://boxlambda.readthedocs.io/en/latest/installation-and-test-builds/).

# Important Note

This project is still in a unreleased state. Until further notice, everything you see here is work-in-progress, in flux, incomplete.
