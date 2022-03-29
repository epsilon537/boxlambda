**BoxLambda** is a open source project with the goal of creating a retro-style FPGA based microcomputer. The microcomputer serves as a platform for software and RTL experimentation.

BoxLambda is a software-hardware cross-over project. The plan is to provide room for experimentation both on the FPGA RTL side and on the software side.

# Key Goals

- Create a sandbox for experimenting in software and (FPGA) HW.    
    - **Simplicity**: It should be easy to jump in and do something: create, hack, tinker.
        - It should be doable for a single person to develop a good understanding of the entire system, software and hardware.
        - **Deterministic Behavior**: By design, it should be clear how long a operation, be it an instruction or a DMA transfer, is going to take.
        - **Single User/Single Tasking OS** booting to a console shell.
    - Create a **Modular Architecture** allowing for a mix-and-match of software and hardware components.
        - Support for partial FPGA reconfiguration.
- Target Hardware is Digilent's [Arty-A7](https://digilent.com/reference/programmable-logic/arty-a7/start) and/or the [Nexys-A7](https://digilent.com/reference/programmable-logic/nexys-a7/start).
- The computer should support following peripherals:
  - Keyboard
  - Mouse (optional)
  - Joystick (optional)
  - Serial port
  - SD card storage
  - VGA Display
  - Audio output
  
- Sound and graphics should be sufficient to support retro-style 2D gameplay.

You can find the GitHub page for BoxLambda at [https://epsilon537.github.io/boxlambda/](https://epsilon537.github.io/boxlambda/).

ddd

