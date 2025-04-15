---
layout: post
title: 'BoxLambda Simplified.'
comments: true
mathjax: yes
---

BoxLambda just got a lot simpler and faster. In this post, I explain why I removed the DMA Controller from the BoxLambda SoC and what that means for the system architecture.

I'll also briefly describe how the RISC-V GNU toolchain for BoxLambda is built.

Recap
-----
[![BoxLambda Block Diagram.](../assets/Arch_Diagram_DFX_w_crossbar.png)](../assets/Arch_Diagram_DFX_w_crossbar.png)

*The BoxLambda Block Diagram before removing the DMA Controller.*

Here's a summary of the current state of BoxLambda (before removing the DMA Controller):
- Targeting the Arty-A7-100T FPGA development board.
- Ibex RISC-V core with machine timer and hardware interrupt support.
- Fixed latency access to CMEM, DMEM, and registers.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- PicoRV32-based Programmable DMA Controller (will be removed in this post).
- Picolibc-based standard C environment.
- DFX Partial FPGA Reconfiguration support.
- Test application suite covering all SoC components, running on both FPGA and Verilator.
- Linux CMake and Bender-based Software and Gateware build system.

Kill Your Darlings
------------------
In the [previous post](https://epsilon537.github.io/boxlambda/latency-shakeup/), I mentioned that wishbone transactions across the crossbar have a high and variable latency (5-6 clock cycles). I got around that by creating shortcuts from the CPU to CMEM and DMEM and by adding transaction separators to stabilize the register access latency. The problem is, those are workarounds, kludges. They make the system more clunky and complicated. It's time to step back and reconsider.

The primary reason for having a crossbar-based interconnect in the BoxLambda SoC is the PicoRV DMA Controller. Without the DMA Controller, the Ibex CPU is essentially the only bus master on the SoC and the crossbar can be replaced with two simple MUX-based buses:
- An Instruction Bus for CPU instruction access.
- A Data Bus access for CPU data access.

A wishbone MUX adds no latency while the wishbone crossbar combined with wb_staller adds 4 clock cycles of latency.

[![CPU read word cycle count to a wishbone slave directly connected, through a mux, and through a crossbar.](../assets/read_word_direct_vs_mux_vs_crossbar.png)](../assets/read_word_direct_vs_mux_vs_crossbar.png)

*CPU read word cycle count to directly connected slave, via MUX, and via crossbar.*

In other words, the price of having a DMA Controller on the BoxLambda SoC is 4 clock cycles of additional transaction latency. This has to be balanced against the value provided by the DMA Controller: moving data around the chip with minimal CPU overhead. I think the price is too high. BoxLambda is supposed to be a simple, low-latency system. I prefer fast access from CPU to VRAM (for example) over having to rely on DMA transfers to get data into or out of VRAM. The PicoRV DMA Controller and the crossbar will have to go.

I spent 3-4 months adding the PicoRV DMA Controller and crossbar to the SoC. I'll have to accept that as Sunk Cost. The decision to keep or cut a body of work should be based on the value it adds, not the amount of time to create it.


BoxLambda Simplified
--------------------
With the PicoRV DMA Controller removed and the crossbar replaced by a MUX-based Instruction and Data bus, the BoxLambda SoC looks like this:

[![BoxLambda SoC with Dual Bus Block Diagram.](../assets/Arch_Diagram_dual_bus_DFX.png)](../assets/Arch_Diagram_dual_bus_DFX.png)

*BoxLambda SoC with Dual Bus Block Diagram.*

This is a very straightforward architecture. A CPU with an instruction and a data port, each connected via their own bus to the slaves they need to be able to reach. The few slaves that have to be hooked up to both buses use a 2-to-1 wishbone arbiter to select between the two buses. In theory, the arbitration will introduce delays when there's concurrent access. In practice, the programmer will know when he's in such a situation and there will be no surprises (or maybe a little one. See the [Arbiters without Overhead](#arbiters-without-overhead-most-of-the-time) section below).

Multiple Bus Masters, but not simultaneously
============================================
OK, I bent the truth a little. Both buses still have multiple bus masters:
- The Data Bus is connected to 3 bus masters: the CPU data port, VS0 (DFX Virtual Socket 0) port 1, and the Debug Module.
- The Instruction Bus is connected to 2 bus masters: the CPU instruction port and VS0 port 0.

[![The Data Bus and the Instruction Bus.](../assets/data_bus_and_instruction_bus.png)](../assets/data_bus_and_instruction_bus.png)

*The Data Bus and the Instruction Bus.*

Both buses have an arbiter that selects which bus master can access the MUX.

I'm retaining the VS0 bus master ports to keep the option of experimenting with alternative CPU designs as DFX Reconfigurable Modules. In such a configuration, the Ibex CPU would go quiet after launching the VS0-based CPU, and VS0 would effectively become the only bus master on the system.

The Debug Module is not active during normal operation.

In other words, during normal operation only one CPU is active. You won't have multiple masters competing for the bus, so register and internal memory access times remain known and constant.

Here is the Instruction and Data bus verilog code:
- [https://github.com/epsilon537/boxlambda/blob/master/gw/components/interconnect/rtl/instruction_bus.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/interconnect/rtl/instruction_bus.sv)
- [https://github.com/epsilon537/boxlambda/blob/master/gw/components/interconnect/rtl/data_bus.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/interconnect/rtl/data_bus.sv)

Arbiters without Overhead (most of the time)
============================================
The wishbone arbiters present a minor problem, however. Arbiters typically introduce some transaction overhead. Luckily, there's a way to avoid that: I added a parameter to the arbiter module that allows you to select a default port. Transactions going through the default port will not suffer arbitration overhead when there are no requests on the other ports. The Bus Master-facing arbiters have their default port connected to the CPU.

[![Arbiter without arbitration overhead on the default port.](../assets/arbiter_wo_overhead.png)](../assets/arbiter_wo_overhead.png)

*Arbiter without arbitration overhead on the default port.*

The arbiter is based on [Alex Forencich's](https://github.com/alexforencich) wishbone arbiter. The arbiter is code-generated using a Python script:

[https://github.com/epsilon537/verilog-wishbone/blob/boxlambda/rtl/wb_arbiter.py](https://github.com/epsilon537/verilog-wishbone/blob/boxlambda/rtl/wb_arbiter.py)

IMEM
====
I replaced the 128KB CMEM and the 128KB DMEM with a single 256KB Dual Port *IMEM*. Having one memory for code and data makes the partitioning of code and data easier and more flexible.

Dual Port VRAM
==============
This isn't visible in the block diagram above, but I made VRAM a dual-port memory. Originally, VRAM was single-port with a time-multiplexed bus for its various clients, the CPU being one of them. I changed it a dual-port memory to give the CPU constant, low latency access to VRAM (2 clock cycles for a load or store operation). VRAM access is now organized as shown in the following diagram:

![Dual Port VRAM](../assets/vera_dp_vram.png)

*Dual Port VRAM.*

Instruction Cycle Counts Summary
================================
All on-chip memory or register accesses are now low-latency with a known, fixed instruction cycle count. Some slaves respond faster than others so the instruction cycle count varies a bit depending on the slave being addressed.

The table below summarizes the instruction cycle counts on BoxLambda according to instruction type and destination.

| Instruction | Destination | Cycles |
|-------------|-------------|--------|
| load/store  | IMEM        | 2      |
|             | VRAM        | 2      |
|             | Sprite RAM  | 2      |
|             | Palette RAM | 2      |
|             | VERA Ctrl   | 2      |
|             | SDRAM       | 15 (variable) |
|             | SDRAM Ctrl  | 3      |
|             | SPIFlash    | 131    |
|             | SPIFlash Ctrl | 2      |
|             | SDSPI       | 3      |
|             | UART        | 3      |
|             | GPIO        | 2      |
|             | USB         | 2      |
|             | ResetCtrl   | 2      |
|             | Timer       | 2      |
|             | YM2149      | 2      |
|             | I2C         | 2      |
| branch      | IMEM        | 2      |
| conditional branch taken |  IMEM  | 4      |
| conditional branch not taken | IMEM   | 2      |
| return from interrupt | IMEM   | 5      |
| integer/computational | IMEM   | 2      |
| multiplication | IMEM   | 2      |
| division/remainder | IMEM       | variable (long division)       |
| CSRSI | IMEM   | 3      |
| CSRS | IMEM   | 4      |
| CSRC | IMEM   | 2      |
| CSRCI | IMEM   | 2      |
| CSRW | IMEM   | 2      |
| CSRR | IMEM   | 2      |

Side Quest: Building a RISC-V GNU Toolchain
-------------------------------------------
Currently, BoxLambda uses whatever RISC-V GNU toolchain happens to be installed on the system. This is problematic. Different compiler versions generate different code and this may impact test results. It may even completely break the system if the compiler happens to include library routines that are built using the compressed RISC-V instruction set (no longer supported on BoxLambda).

I would like to include a RISC-V GNU toolchain, specifically built for BoxLambda, as part of the BoxLambda distribution. The toolchain should be built so that the executables can run out of the box on different x86_64 Linux systems (i.e. without relying on system-specific shared libraries).

[Crosstool-ng](https://crosstool-ng.github.io/) makes this task surprisingly easy. The tool uses a menuconfig similar to the Linux kernel menuconfig:

![Crosstool-ng menuconfig](../assets/ct-ng-menuconfig.png)

*Crosstool-NG menuconfig.*

You just focus on the specifics of the toolchain you want to build (RISC-V, 32-bit, Static Toolchain...). The tool selects good defaults for all the rest.

I selected:
- Target Architecture: riscv
- Architecture level: rv32im_zicsr_za_zb_zbs
- ABI: ilp32
- Build Static Toolchain
- Tuple's vendor string: boxlambda
- Target OS: bare-metal
- Additional support languages: C++

After `ct-ng menuconfig` you type `ct-ng build` and the tool goes off fetching the necessary repos and tarballs, applying patches, and building the whole thing. The result is a *riscv32-boxlambda-elf/* folder organized according to the typical GNU toolchain directory structure:

```
riscv32-boxlambda-elf/
├── bin
├── include
├── lib
│   ├── bfd-plugins
│   └── gcc
├── libexec
│   └── gcc
├── riscv32-boxlambda-elf
│   ├── bin
│   ├── include
│   ├── lib
│   ├── sys-include
│   └── usr
└── share
    └── licenses
```

The toolchain tarball is checked into the BoxLambda repo. The [boxlambda_setup.sh](https://github.com/epsilon537/boxlambda/blob/master/boxlambda_setup.sh) script unpacks the toolchain tarball in the *boxlambda/tools/* directory, so the user no longer needs to provide the toolchain as a prerequisite.

Try It Yourself
---------------

Setup
=====

[Install the prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).

1. Get the repository:
  ```
  git clone https://github.com/epsilon537/boxlambda/
  cd boxlambda
  ```

2. Switch to the **boxlambda_simplified** tag:
    ```
    git checkout boxlambda_simplified
    ```

3. Set up the environment:
  ```
  source boxlambda_setup.sh
  ```

4. Activate the environment:
  ```
  source activate_env.sh
  ```

  The first three steps only need to be executed once. Activating the environment is required every time you're working with BoxLambda.

### Peeking Words with the DFX test program on FPGA

The [peekw CLI command](https://github.com/epsilon537/boxlambda/blob/master/sw/components/peek_poke_cli/peek_poke_cli.cpp), in addition to retrieving the value of the given register or memory location, measures the instruction cycle count of the load word (*lw*) transaction. This can be used to measure how long it takes to read a word from specific slaves (IMEM, UART...).

Hook up the MicroSD PMOD as described [here](https://boxlambda.readthedocs.io/en/latest/pmods/#microsd-pmod) and insert a FAT-formatted SD card.

Connect a terminal emulator to Arty's USB serial port. I suggest using a terminal emulator that supports Ymodem transfers such as *Minicom*. **Settings: 115200 8N1**.

Build the *dfx_test_flsh* software project in the arty-a7-100 build tree:

```
cd build/arty-a7-100/sw/projects/dfx_test
make dfx_test_flsh
```

Flash the *dfx_test_flsh* program onto the target:

```
make dfx_test_flsh_flash_sw
```

Build the *boxlambda_dfx* gateware project in the *arty-a7-100* build tree:

```
cd build/arty-a7-100/gw/projects/boxlambda_dfx
make boxlambda_dfx_bit
```

Flash the gateware build onto the target:

```
make boxlambda_dfx_flash_gw
```

When flashing has been completed, the target should boot up. You should see the following messages:

```
Starting...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  m0, b01: |11111111111111111111111111111100| delays: 14+-14
  m0, b02: |00000000000000000000000000000000| delays: -
  m0, b03: |00000000000000000000000000000000| delays: -
  m0, b04: |00000000000000000000000000000000| delays: -
  m0, b05: |00000000000000000000000000000000| delays: -
  m0, b06: |00000000000000000000000000000000| delays: -
  m0, b07: |00000000000000000000000000000000| delays: -
  best: m0, b01 delays: 14+-14
  m1, b00: |00000000000000000000000000000000| delays: -
  m1, b01: |11111111111111111111111111111100| delays: 14+-14
  m1, b02: |00000000000000000000000000000000| delays: -
  m1, b03: |00000000000000000000000000000000| delays: -
  m1, b04: |00000000000000000000000000000000| delays: -
  m1, b05: |00000000000000000000000000000000| delays: -
  m1, b06: |00000000000000000000000000000000| delays: -
  m1, b07: |00000000000000000000000000000000| delays: -
  best: m1, b01 delays: 14+-14
Switching SDRAM to hardware control.
SDRAM init OK.
Mounting filesystem...
CID: 534d5402:47323341:7d604971:3168018d
Reading VS0 core signature register...
Read signature value: 0x510b
Starting CLI...
```

Enter a *peekw* command, e.g. read a UART register:

```
> peekw 10010000
peekw 0x10010000 -> 0x400001B2
Cycles: 3
```

Conclusion
----------

The new block diagram is quite similar to the [initial architecture](https://epsilon537.github.io/boxlambda/architecture-first-draft/) diagram I proposed when I set out on this project. I drifted away from it over time and then gravitated back. It's a straightforward architecture that meets BoxLambda's predictability and simplicity requirements.

Are we done talking about latency yet? Almost. We haven't discussed interrupt latency yet. That'll be the topic of the next post.

References
----------
[Crosstool-ng](https://crosstool-ng.github.io/): An excellent tool for building cross-compiler GNU toolchains.

