---
layout: post
title: 'The Latency Shakeup.'
comments: true
mathjax: yes
---

*Updated 13 April 2025: I corrected the breakdown of the 6 clock cycle register access time in [this](#the-single-instruction-prefetcher) section.*

All gateware components are now in place on the BoxLambda SoC, but the system is not yet behaving as required. A key requirement of BoxLambda is deterministic behavior. The duration of operations such as internal memory or register access must be predictable by design. When analyzing a snippet of assembly code, you should be able to predict exactly how many clock cycles it will take to execute, without relying on statistics. That's what this post is about.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_NoDFX.png)

Here's a summary of the current state of BoxLambda:
- An Ibex RISC-V core with machine timer and hardware interrupt support.
- Wishbone interconnect and Harvard architecture internal memory.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- A PicoRV32-based Programmable DMA Controller.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- DFX Partial FPGA Reconfiguration support.
- A *Base* and a *DFX* configuration targeting the Arty-A7-100T FPGA development board.
- A suite of test applications covering all SoC components, running on both FPGA and Verilator.
- Automated testing on Verilator and CocoTB.
- A Linux CMake and Bender-based Software and Gateware build system.

Behavior of a Simple Word Copy Loop Before Adjustments
------------------------------------------------------
Let's look at the behavior of the Ibex CPU's Instruction Fetch (IF) and Instruction Decode (ID) stage in the case of a simple word copy loop.

This is the loop's disassembly:

```
34c:       0004a283                lw      t0,0(s1)
350:       00542023                sw      t0,0(s0)
354:       0411                    addi    s0,s0,4
356:       0491                    addi    s1,s1,4
358:       17fd                    addi    a5,a5,-1
35a:       fbed                    bnez    a5,34c <lw_sw_copy_loop+0x1a>
```

Here are some relevant signals from the Ibex IF and ID stages making one pass through the loop:

[![Before making any system changes, Ibex IF and ID stage executing a word copy loop.](../assets/word_copy_loop_compressed_instr.png)](../assets/word_copy_loop_compressed_instr.png)

*Before making any system changes, Ibex IF and ID stage executing a word copy loop (click to zoom).*

Pipeline Signals
================
The following signals illustrate the behavior of the Ibex two-stage pipeline:
- **prefetch_buffer_i.instr_\*** signals show the Instruction Prefetcher fetching instructions from memory:
  - **instr_addr_o**: instruction address output.
  - **instr_req_o**: instruction request output strobe.
  - **instr_rvalid_i**: instruction return data valid input strobe.
- **prefetch_buffer.addr_o/valid_o** shows the IF stage handing over the next instruction to the ID stage.
- **id_stage.pc_i** shows the ID stage's program counter, i.e. the address of the instruction being executed.
- **id_stage.instr_is_compressed** indicates if the instruction being executed is compressed.
- **id_stage.id_in_ready_o:**: This signal indicates when the ID stage is ready to receive the next instruction from the IF stage. If the ID stage stalls (e.g. due to a load operation in the Load-Store unit), *id_ready_o* is deasserted until the ID stage is ready to proceed.
- **id_stage.instr_executing** indicates if the ID stage is currently executing an instruction.

Execution Irregularities
========================
The *pc_id_i* signal shows how the ID stage progresses through the loop. Notice that similar instructions don't have different execution times:

  - the *lw* (load word) instruction takes longer to execute than the *sw* (store word) instruction.
  - the 2nd *addi* (add immediate) instruction takes longer than the first and third addi instruction.

Additionally, the branch instruction takes a long time to complete.

The goal is to analyze this behavior and adjust the system so that execution timing can be predicted directly from the code, without requiring waveform inspection.

16-bit and 32-bit instructions
------------------------------
There are several factors at play here. To start with, the word copy loop contains a mix of 16-bit and 32-bit instructions, but the instruction Prefetcher always fetches 32-bit words at a time. A prefetched 32-bit word can contain one of the following:
- Two 16-bit instructions.
- One 32-bit instruction.
- One 16-bit instruction and the first half of a 32-bit instruction.
- The 2nd half of a 32-bit instruction and a 16-bit instruction.
- The 2nd half of a 32-bit instruction and the 1st half of another 32-bit instruction.

All of these combinations have an impact on the cycle count of the instructions involved. This impact is predictable based on the instruction sequence. However, I prefer to keep things simple (another core BoxLambda requirement), so I'm going to switch to all uncompressed instructions by setting *CFLAG -march* to *rv32im* instead of *rv32imc*:

```
     41c:       0004a283                lw      t0,0(s1)
     420:       00542023                sw      t0,0(s0)
     424:       00440413                addi    s0,s0,4
     428:       00448493                addi    s1,s1,4
     42c:       fff78793                addi    a5,a5,-1
     430:       fe0796e3                bnez    a5,41c <lw_sw_copy_loop+0x30>
```

The assembly code is the same as before, but all instructions are 32-bit now.

The waveform of the CPU IF and ID stages making one pass through this code now looks like this:

[![Ibex IF Prefetcher and ID stage executing a word copy loop. All 32-bit instructions](../assets/word_copy_loop_uncompressed_instr.png)](../assets/word_copy_loop_uncompressed_instr.png)

*Ibex IF Prefetcher and ID stage executing a word copy loop. All 32-bit instructions (click to zoom).*

It's now easier to match up what's going on in the ID stage with the IF stage just before. There are still irregularities in the instruction cycle counts, however. Also, the instruction cycle counts are quite high.

Bypassing the Crossbar
----------------------
Looking at the Architecture Block Diagram at the beginning of the post, you'll see that Instruction fetches and data access go through the Wishbone Crossbar. There are two problems with this approach:

- The Crossbar prioritizes throughput over latency. It can move a lot of data in systems that support many outstanding transactions. In the case of setups such as BoxLambda, however, where each transaction is blocking, the Crossbar is slow.
- There is a one-clock cycle cost for *channel switching*. An instruction fetch transaction completes faster when executed back-to-back with the previous transaction, without deasserting CYC. However, if the ID stage stalls the IF stage, preventing back-to-back IF transactions, the instruction fetch transaction right after the stall will take one clock cycle longer. This causes irregularities in the instruction cycle counts.

To sidestep these issues, we can give the CPU direct access to the internal memories instead of going through the Crossbar, like so:

![Ibex direct access to internal memory, bypassing the Crossbar](../assets/crossbar_bypass.png)

*Ibex direct access to internal memory, bypassing the Crossbar*

The waveform now looks like this:

[![Ibex IF Prefetcher and ID stage executing a word copy loop. Direct CPU access to internal memory](../assets/word_copy_loop_crossbar_bypass.png)](../assets/word_copy_loop_crossbar_bypass.png)

*Ibex IF Prefetcher and ID stage executing a word copy loop. Direct CPU access to internal memory (click to zoom).*

This is much faster and very regular:

- 2 clock cycles to read a word from internal memory.
- 2 clock cycles to write a word to internal memory.
- 2 clock cycles for each addi instruction.
- 4 clock cycles for a branch taken.

There is more to a system than internal memory access, however.

The 1->13 MUX and Transaction Separators
-----------------------------------------
Let's consider peripheral register access. The CPU reading a register from, for instance, the I2C core involves a Wishbone transaction passing through *two* crossbar instances: the main crossbar and the shared bus crossbar:

![CPU access to I2C, passing through two crossbars](../assets/cpu_access_to_i2c.png)

*CPU access to I2C, passing through two crossbars.*

So in the case of peripheral register access, we're dealing with the same crossbar issues discussed in the previous section, times two. There are two things we can do to improve the situation:

1. Replace the shared bus *wbxbar* instance with a 1->13 MUX, an extended version of the 1->2 MUX we're already using at the processor ports. This MUX does not introduce any transaction latency.
2. We can get rid of the channel switching irregularities by inserting **Transaction Separators**, or *wb_stallers*. These transaction separators deassert CYC for one clock cycle between transactions so there are no back-to-back transactions. This increases the transaction latency going through the main crossbar with one clock cycle, but it removes the channel switching irregularities.

![BoxLambda SoC with two transaction separators and 1->13 MUX](../assets/stallers_and_1_13_mux.png)

*BoxLambda SoC with two transaction separators and 1->13 MUX.*

![Transaction Separator Signals: wbm_\* signals are bus master facing, wbs_\* signals are bus slave facing](../assets/wb_staller_waveform.png)

*WB_Staller Transaction Separator signals: wbm_\* signals are bus master facing, wbs_\* signals are bus slave facing.*

The above waveform shows instruction fetch transactions from DMEM. These transactions go through the crossbar. The waveform shows that it takes 1 clock cycle to go through the *wb_staller* (CYC deasserting for 1 clock cycle) and 5 clock cycles (STB->ACK) to go through the crossbar. Based on this data, I expect that a peripheral register read operation will take 6 clock cycles.

The Single Instruction Prefetcher
---------------------------------
With the 1->13 MUX and Transaction Separators in place, let's look at the behavior of the IF Prefetcher and ID stage continuously reading an I2C register in a loop. This is the disassembly of the loop:

```
     3ac:       00042283                lw      t0,0(s0)
     3b0:       fff78793                addi    a5,a5,-1
     3b4:       fe079ce3                bnez    a5,3ac <lw_register_loop+0x28>
```

And this is the waveform:

![CPU with default FIFO-based Prefetcher reading an I2C register in a loop](../assets/register_read_loop_default_prefetcher.png)

*CPU with default FIFO-based Prefetcher reading an I2C register in a loop.*

It looks like we're not there yet. In this loop, the *addi* (add immediate) instruction only takes one clock cycle. In the word copy loop earlier, the *addi* instruction takes 2 clock cycles. A bit of investigation shows that this irregularity comes from the Instruction Prefetcher. The Instruction Prefetcher contains a small FIFO that caches prefetched instructions. This creates a bit of elasticity. Generally, that's a good thing, but not if you want predictable instruction cycle counts.

I wrote an alternative, **Single Instruction Prefetcher** module that, as the name says, just prefetches a single instruction. The Single Instruction Prefetcher can be used as a drop-in replacement for the Ibex processor's default Instruction Prefetcher. You select it by setting the *PrefetchType* parameter to *PrefetchType_Single* in the instantiation of the wb_ibex_core:

```
  wb_ibex_core #(
      .RV32M(ibex_pkg::RV32MSingleCycle),
      .RV32B(ibex_pkg::RV32BBalanced),
      .RegFile(`PRIM_DEFAULT_IMPL == prim_pkg::ImplGeneric ? ibex_pkg::RegFileFF : ibex_pkg::RegFileFPGA),
      .PrefetchType(ibex_pkg::PrefetchType_Single),
      .BranchTargetALU(1'b0), //I would like to enable this, but I'm running into timing closure issues if I do.
      .WritebackStage(1'b0),
      .DbgTriggerEn(1'b1),
      .DmHaltAddr({2'b00, SHARED_BUS_SLAVE_ADDRS[(DM_S+1)*AW-1:DM_S*AW], 2'b00} + 32'h00000800),
      .DmExceptionAddr({2'b00, SHARED_BUS_SLAVE_ADDRS[(DM_S+1)*AW-1:DM_S*AW], 2'b00} + 32'h00000808)
  ) wb_ibex_core (
```

![The Single Instruction Prefetch Buffer in the Ibex Core.](../assets/ibex_single_prefetch_buffer.png)

*The Single Instruction Prefetch Buffer in the Ibex Core.*

This prefetcher is very simple. It prefetches one 32-bit (i.e. uncompressed) instruction and delivers it to the Instruction Fetch (IF) stage when that stage indicates it is *ready*. The prefetched address is either the previously fetched address incremented by 4 (for linear execution) or the address specified by the IF stage in the case of a *branch_i* request.

The Single Instruction Prefetch SystemVerilog code can be found here:

[https://github.com/epsilon537/ibex/blob/boxlambda/rtl/ibex_single_prefetch_buffer.sv](https://github.com/epsilon537/ibex/blob/boxlambda/rtl/ibex_single_prefetch_buffer.sv)

Using the Single Instruction Prefetcher, the waveform corresponding to the register read loop looks like this:

![CPU with Single Instruction Prefetcher reading an I2C register in a loop](../assets/register_read_loop_single_prefetcher.png)

*CPU with Single Instruction Prefetcher reading an I2C register in a loop.*

This looks good. The instruction cycle counts are what we expected:
- 6 clock cycles for a peripheral register read operation: 1 to issue the read request, 1 to go through the transaction separator, 3 to go through the crossbar, and 1 for the slave to respond.
- 2 clock cycles for the *addi* instruction.
- 4 clock cycles for a branch taken.

The Price Paid
==============
While the instruction cycle count of the register read loop is now predictable, this comes at a cost. With the Single Instruction Prefetcher, it takes 12 clock cycles to make one pass through the loop vs. 9 clock default with the FIFO-based Prefetcher. This increase occurs because the Single Instruction Prefetcher requires two clock cycles to fetch each instruction from CMEM, and it does not utilize caching. Instruction Fetch (including Prefetch) and Instruction Decode are two stages in a pipeline. This pipeline puts a lower bound on the instruction cycle count. Even though the ID stage can execute some instructions (e.g. *addi*) in 1 clock cycle, it receives instructions at a maximum rate of 1 instruction every 2 clock cycles.

Another downside of the Single Instruction Prefetcher is that it only supports uncompressed instructions. We can no longer just compile critical code sections as uncompressed instructions. *All* code has to be compiled into uncompressed instructions.

Branches
========
It takes 4 clock cycles to branch. You can see in the waveform why this is so. Executing the *bnez* instruction itself (pc_id_i=0x3b4) takes two clock cycles. After executing the *bnez* instruction, the pipeline is primed to execute the next sequential instruction, at 0x03b8. It can't execute that instruction, however. It has to branch back to the beginning of the loop. The instruction at 0x03b8, ready to go, is discarded. The *instr_executing* signal goes low until execution resumes at address 0x3ac.

Ibex has a feature that reduces branch execution time by including a 2nd ALU that is used exclusively for branch target computations. Unfortunately, if I enable this feature I can no longer close timing at 50MHz.

VRAM Access
-----------
One more waveform and we'll call it a day. This is the same word copy routine as before, but this time it's copying words in VRAM instead of DMEM:

```
     41c:       0004a283                lw      t0,0(s1)
     420:       00542023                sw      t0,0(s0)
     424:       00440413                addi    s0,s0,4
     428:       00448493                addi    s1,s1,4
     42c:       fff78793                addi    a5,a5,-1
     430:       fe0796e3                bnez    a5,41c <lw_sw_copy_loop+0x30>
```

[![CPU with Single Instruction Prefetcher copying words in VRAM](../assets/word_copy_loop_vram.png)](../assets/word_copy_loop_vram.png)

*CPU with Single Instruction Prefetcher copying words in VRAM (click to zoom).*

The *lw* instruction takes 6 clock cycles while the *sw* takes 8 clock cycles. What is it this time? Here the issue is that VRAM is a single-port RAM with multiple clients taking turns to get access to that RAM.

![Vera VRAM Interface](../assets/vera_vram_if.png)

*The VERA VRAM Interface.*

Access to VRAM is arranged into time slots. If you write the VRAM accessing code just so, you may get it to fall into a beat pattern which makes the whole thing predictable again. This is too complex and slow, however. A better solution is needed. That's a topic for the next post.

Other Changes
-------------
See [CHANGELOG.md](https://github.com/epsilon537/boxlambda/blob/master/CHANGELOG.md).

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

2. Switch to the **latency_shakeup** tag:
    ```
    git checkout latency_shakeup
    ```

3. Set up the environment:
  ```
  source boxlambda_setup.sh
  ```

4. Activate the environment:
  ```
  source activate_env.sh
  ```

  The first three steps only need be executed once. Activating the environment is required every time you're working with BoxLambda.

## The Ibex Performance Test on Verilator

The value of this test is mostly in the waveform it generates. Checking the waveform of this test allows you to see instruction fetch latency and the cycle count of various common instructions: load-word and store-word to internal memory and VRAM, reading SoC registers, addi, branch taken, branch not taken.

The test program consists of the following sub-tests:

- **do_nothing()**: Measures how many cycles it takes to call *mcycle_start()* and *mcycle_stop()*.
- **lw_register_loop()**: Repeatedly reads a peripheral register and measures how long it takes.
- **lw_sw_copy_loop()**: Copies 100 words from DMEM to DMEM, or from VRAM to VRAM, using a naive loop and measures how long it takes.
- **lw_sw_copy_loop_unrolled()**: Same as the previous test, but with the loop unrolled.

Build the **ibex_perf_test** project:

```
cd build/sim-a7-100/gw/projects/ibex_perf_test
make ibex_perf_test_sim_sw
```

Execute the generated Verilator model with tracing enabled (*-t*). You should see the following output:

```
./Vmodel -t
SIM: Tracing enabled
DUT: Do nothing: 8 cycles.
DUT: Expected: 8 cycles.
DUT: lw_sw_register_loop: addr: 0x10000208, 12 cycles/iteration.
DUT: Expected: 12 cycles.
DUT: lw_sw_copy_loop: dest: 0x24b68, src: 0x20b68, 14 cycles/iteration.
DUT: Expected: 14 cycles.
DUT: lw_sw_copy unrolled: 8 cycles/iteration.
DUT: Expected: 8 cycles.
DUT: lw_sw_copy_loop: dest: 0x12040190, src: 0x12044190, 24 cycles/iteration.
DUT: Expected: TBD.
DUT: Test Successful.
SIM: Test passed.
```

Open the generated waveform file with GTKWave:

```
gtkwave simx.fst
```

Some interesting signals to monitor are:

```
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.id_stage_i.controller_i.clk_i
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.id_stage_i.controller_i.pc_id_i[31:0]
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.id_stage_i.controller_i.id_in_ready_o
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.id_stage_i.controller_i.instr_exec_i
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.addr_i[31:0]
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.addr_o[31:0]
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.branch_i
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.ready_i
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.valid_o
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.instr_addr_o[31:0]
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.instr_gnt_i
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.instr_req_o
TOP.sim_main.dut.boxlambda_soc_inst.wb_ibex_core.u_top.u_ibex_core.if_stage_i.gen_single_prefetch_buffer.single_prefetch_buffer_i.instr_rvalid_i
```

[![Ibex Performance Test Waveform in GTKWave](../assets/ibex_perf_test_gtkwave_screenshot.png)](../assets/ibex_perf_test_gtkwave_screenshot.png)

*Ibex Performance Test Waveform in GTKWave (click to zoom).*

To navigate the waveform, check the ID stage's *pc_id_i* values against the addresses in the disassembly of the *ibex_perf_test_ram* executable.

## Ibex Performance Test on Arty A7

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Build the project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/ibex_perf_test
make ibex_perf_test_bit_sw
```

Download the generated bitstream file to the Arty A7:

```
make ibex_perf_test_load
```

In the terminal emulator, you should see the same output as in the Verilator test build above.

Conclusion
----------

We made good progress toward achieving BoxLambda's Deterministic Behavior requirement. It's now much easier to predict the exact instruction cycle count of a chunk of code. Additionally, the overall latency of the system has been significantly reduced. However, the goal has not yet been fully achieved. VRAM access remains an issue, and other aspects of deterministic behavior, such as interrupt latency, have yet to be addressed. And what about that other core requirement: Simplicity? Is BoxLambda simple enough? I see significant system changes coming up. To be continued.

References
----------
[Ibex Documentation](https://ibex-core.readthedocs.io/en/latest)

