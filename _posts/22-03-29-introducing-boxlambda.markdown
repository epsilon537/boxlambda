---
layout: post
title: Introducing the BoxLambda Project.
comments: true
---

Alright, this is it. We're live.
I'm starting an project called BoxLambda. Here's the run-down, copied verbatim from the README.md:

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

You can find the source code for BoxLambda at GitHub: [https://github.com/epsilon537/boxlambda/](https://github.com/epsilon537/boxlambda/).

Why?
---
Does the world need another retro-style computer? Probably not, but I do. I'm a software engineer and I've been studying FPGA development for about a year now, specifically with the goal of starting this project.

It's an ambitious project and at least half of it (the FPGA half) is in a realm with which I have very little experience. I don't know if the project will succeed. Maybe I'm too ambitious and too naive. We'll see. This Blog will document the journey.

OK, that's enough for a first post I think. See you in the next one!

