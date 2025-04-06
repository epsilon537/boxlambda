---
layout: post
title: 'Minimizing Interrupt Latency and Jitter'
comments: true
mathjax: yes
---

In this post, I will be looking at ways to improve Interrupt Latency and Jitter on the BoxLambda SoC. I will also highlight other system changes made to improve latency across the SoC and wrap up with a table of instruction cycle counts according instruction type and their destination.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_dual_bus_DFX.png)

Here's a summary of the current state of BoxLambda:
- Targeting the Arty-A7-100T FPGA development board.
- An Ibex RISC-V core with machine timer and hardware interrupt support.
- Harvard architecture based interconnect.
- Low latency register and memory access across the SoC.
- Predictable instruction cycle counts.
- DFX Partial FPGA Reconfiguration support.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller, I2C Controller.
- Real-time Clock and Calendar (RTCC) support.
- USB HID Keyboard and Mouse support.
- A Picolibc-based standard C environment.
- A suite of test applications covering all SoC components, running on both FPGA and Verilator.
- A Linux CMake and Bender-based Software and Gateware build system.

Before diving into interrupts, I want to point out that the Architecture has changed quite a bit since the previous post. The upshot of the change, relevant for this post, is that CPU access to internal memory and timer registers take two clock cycles to complete.

I'll summarize the changes in the (Other Changes)[] section below, after covering the main topics of this post.

Interrupt Latency Before Adjustments
------------------------------------
Let's have a look at the waveform of a timer interrupt firing:

[![Waveform of a Timer Interrupt Firing.](../assets/timer_irq_waveform_before_changes.png)](../assets/timer_irq_waveform_before_changes.png)

*Waveform of a Timer Interrupt Firing.*

You see the time IRQ firing while the CPU is executing the instruction at address 0x02ec. Five clock cycles later the CPU branches to the IRQ vector at 0x1c and two clock cycles after that we're in the timer interrupt service routine (ISR). Six clock cycles interrupt latency. Not too bad, you might say. However, the actual damage is yet to come. This is the source code of the timer ISR:

```
void _timer_irq_handler(void) {
  mtimer_disable_raw_time_cmp();
  timer_irq_fired = 1;
}
```

The ISR disables the timer and sets a flag. This is the corresponding disassembly:

```
0000090c <_timer_irq_handler>:
     90c:       fc010113                addi    sp,sp,-64
     910:       00e12e23                sw      a4,28(sp)
     914:       00f12c23                sw      a5,24(sp)
     918:       02112e23                sw      ra,60(sp)
     91c:       02512c23                sw      t0,56(sp)
     920:       02612a23                sw      t1,52(sp)
     924:       02712823                sw      t2,48(sp)
     928:       02a12623                sw      a0,44(sp)
     92c:       02b12423                sw      a1,40(sp)
     930:       02c12223                sw      a2,36(sp)
     934:       02d12023                sw      a3,32(sp)
     938:       01012a23                sw      a6,20(sp)
     93c:       01112823                sw      a7,16(sp)
     940:       01c12623                sw      t3,12(sp)
     944:       01d12423                sw      t4,8(sp)
     948:       01e12223                sw      t5,4(sp)
     94c:       01f12023                sw      t6,0(sp)
     950:       0fc000ef                jal     a4c <mtimer_disable_raw_time_cmp>
     954:       00100713                li      a4,1
     958:       5ae1a823                sw      a4,1456(gp) # 20df0 <timer_irq_fired>
     95c:       03c12083                lw      ra,60(sp)
     960:       03812283                lw      t0,56(sp)
     964:       03412303                lw      t1,52(sp)
     968:       03012383                lw      t2,48(sp)
     96c:       02c12503                lw      a0,44(sp)
     970:       02812583                lw      a1,40(sp)
     974:       02412603                lw      a2,36(sp)
     978:       02012683                lw      a3,32(sp)
     97c:       01c12703                lw      a4,28(sp)
     980:       01812783                lw      a5,24(sp)
     984:       01412803                lw      a6,20(sp)
     988:       01012883                lw      a7,16(sp)
     98c:       00c12e03                lw      t3,12(sp)
     990:       00812e83                lw      t4,8(sp)
     994:       00412f03                lw      t5,4(sp)
     998:       00012f83                lw      t6,0(sp)
     99c:       04010113                addi    sp,sp,64
     9a0:       30200073                mret
```

That's 38 instructions, not including the routine that disables the timer! Where does all that code come from? Frome the *interrupt("machine")* attribute in the ISR declaration:

```
void _timer_irq_handler(void) __attribute__((interrupt("machine")));
```

The CPU register needs to be saved before the ISR can make any register changes and restored when the ISR has done its work. This simple timer ISR takes 101 clock cycles to execute and it takes 43 clock cycles from the risinge edge of the timer IRQ signal until the execution of the first non-prologue instruction in the ISR.

Interrupt Shadow Registers
--------------------------
To reduce the ISR prologue and epilogue overhead, I added an interrupt shadow register bank to the CPU's register file:

![The Ibex Register File including a Interrupt Shadow Register Bank](../assets/irq_shadow_registers.png)

Following the pattern of Ibex's *nmi_mode*, I added an *irq_mode* signal that tracks when the CPU enters and exits an ISR. This is just a 1-bit signal. It can't track nested interrupts, which is fine for BoxLambda.

The Register File has two register banks:
- The *thread-level* bank named *mem*
- The *interrupt-level* bank named *irq_mem*.

The *irq_mode* signal controls how register reads and writes are handled in the Register File:
- When the CPU is running at thread-level (i.e. not in IRQ mode), register writes are stored in both register banks and register reads are fetched from the *mem* bank.
- When the CPU is running in IRQ mode, register writes are only stored in the *irq_mem* register bank and register reads are fetched from the *irq_mem* bank.

With this design, the *irq_mem* bank is up-to-date when the CPU enter IRQ mode. Any register writes while in IRQ mode go to the *irq_mem* bank only and don't affect the thread-level *mem* bank. This means that no registers need to be saved or restored when switching between thread-level mode and IRQ mode.

But how do we tell the compiler that it can skip the prologue and epilogue? Interestingly, the GCC compiler has a **naked** attribute that does exactly this.

```
void __attribute__((naked)) _timer_irq_handler(void);
```

Because the *naked* attribute skips all epilogue, we have to insert the *mret* instruction ourselves in the ISR:

```
void _timer_irq_handler(void) {
  mtimer_disable_raw_time_cmp();
  timer_irq_fired = 1;

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}
```

The disassembly looks like this:

```
0000070c <_timer_irq_handler>:
     70c:       0b8000ef                jal     7c4 <mtimer_disable_raw_time_cmp>
     710:       00100713                li      a4,1
     714:       4ae1a223                sw      a4,1188(gp) # 536c <timer_irq_fired>
     718:       30200073                mret
```

Now it takes 27 clock cyles to execute the ISR (down from 101 clock cycles) and takes it take 6 clock cycles from the risinge edge of the timer IRQ signal until the execution of the first timer ISR specific instruction in the ISR (down from 43 clock cycles).

| Timer ISR | Without IRQ Shadow Registers | With IRQ Shadow Registers |
|-----------|------------------------------|---------------------------|
| IRQ Latency | 43 clock cycles | 6 clock cycles |
| ISR Prologue and Epilogue overhead | 82 clock cycles| 7 clock cycles |

IRQ Jitter
----------
The six clock cycle IRQ latency is only partially true. The actual latency depends on the instruction being executed when the IRQ occurs. The CPU has to complete that instruction before it can jump to the IRQ vector. This constraint causes some IRQ latency jitter. For asynchronous events such as keypresses that won't matter. However, for timed events there may be cases where you want to execute a sequence of instructions *exactly* at a given time, without any jitter.

To accommodate such a use case, I added an MTIMEBLK register to the Timer core. A write operation to this register blocks until the lower 8 bits of the MTIMER register matches the written value. This mechanism can be used to absorb Timer IRQ jitter as shown in the diagram below:

![Absorbing Timer IRQ Jitter with MTIMEBLK](../assets/mtimeblk_mechanism.png)

*Absorbing Timer IRQ Jitter with MTIMEBLK.*

You program a Timer IRQ to fire a few clock cycles before the desired time. Then, in the timer ISR, you write to MTIMEBLK to block until the desired time is reached exactly.

The [timer_uart_gpio_irqs]() test implements this mechanism:

```
volatile uint32_t timer_irq_expected_at = 0;
volatile uint32_t mtime_after_irq_jitter_removal = 0;

void _timer_irq_handler(void) {
  /* MTIMER_BLK_UNTIL acts as an IRQ jitter absorber: _timer_irq_handler is entered slightly */
  /* before the expected time, with a bit of uncertainty due to IRQ jitter. */
  /* MTIMER_BLK_UNTIL then blocks the system for a few more cycles, until we exactly reach the expected time. */
  MTIMER_BLK_UNTIL(timer_irq_expected_at-1); //Block until 1 clock cycle before the expected time.
  mtime_after_irq_jitter_removal = MTIMER_GET_RAW_MTIME_LOW(); //Retrieve the mtime value.

  //Stop the timer. If we don't stop it, or move into the future, the IRQ will keep on firing.
  mtimer_disable_raw_time_cmp();
  timer_irq_fired = 1;

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

int main(void) {
  ...
  printf("Setting timer...\n");

  timer_irq_fired = 0;
  mtimer_set_raw_time_cmp(5000); //Fire IRQ in 5000 ticks.
  //Timer IRQ firing time + irq jitter absorption margin
  timer_irq_expected_at = MTIMER_GET_RAW_TIMECMP_LOW()+IRQ_LATENCY_AND_JITTER_MARGIN;
  printf("Timer IRQ expected at: %d\n", timer_irq_expected_at);

  while (timer_irq_fired == 0); //Wait for it.

  //Minus two because it takes two clock cycle to read the mtime in the isr.
  printf("Timer ISR after IRQ jitter removal started at: %d\n", mtime_after_irq_jitter_removal-2);

  if (mtime_after_irq_jitter_removal - 2 != timer_irq_expected_at) {
    printf("Test failed.\n");
    return -1;
  }
```

This mechanism can also be used to remove jitter from periodic interrupts such VGA line IRQs.

Architectural Changes
---------------------
Kill your Darlings
==================
Coming back to the architectural changes: I removed the DMA Controller from the SoC. The DMA Controller added a lot of complexity to the design and it was the only reason the design required a crossbar. That crossbar in turn introduced a lot of latency in the SoC. If the cost of a DMA Controller is a doubling of the latency inside a SoC, it's probably better not to have a DMA Controller.

The Dual Bus
============
With the DMA Controller removed, I could replace the crossbar with a dual bus: an Instruction Bus and an Data Bus. While each bus has multiple bus masters, during normal operation only the CPU is active so you won't have multiple masters competing for the bus and register and memory access times remain known and constant.

IMEM
====
I replaced 128KB CMEM and 128KB DMEM with a single 256KB Dual Port IMEM. Having one memory for code and data makes the partitioning of code and data easier and more flexible.

Dual Port VRAM
==============
VRAM was a single port memory with a time-multiplexed bus for its various clients. I changed it a dual port memory to give the CPU constant, low latency (2 clock cycles) access to VRAM.

![Dual Port VRAM](../assets/vera_dp_ram.png)

*Dual Port VRAM.*

Instruction Cycle Counts Summary
================================
While latency from the CPU to a given slave (memory, register) is uniform across the SoC, some slaves respond a little faster than others. The table below summarizes the instruction cycle counts on BoxLambda according to instruction type and destination.

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

  The first three steps only need be executed once. Activating the environment is required every time you're working with BoxLambda.

## The Ibex Performance Test on Verilator

The [Ibex Performance Test]() now includes an IRQ latency measurement test.

Build the **ibex_perf_test** project:

```
cd build/sim-a7-100/gw/projects/ibex_perf_test
make ibex_perf_test_sim
```

Execute the generated Verilator model. You should see the following output:

```
./Vmodel
DUT: Enabling Timer IRQ.
DUT: Timer IRQ latency Min-Max: 5-7 cycles.
DUT: Expected: 5-7 cycles.
DUT: Do nothing: 8 cycles.
DUT: Expected: 8 cycles.
DUT: lw_sw_register_loop: addr: 0x10000208, 8 cycles/iteration.
DUT: Expected: 8 cycles.
DUT: lw_sw_copy_loop: dest: 0x96c4, src: 0x56c4, 14 cycles/iteration.
DUT: Expected: 14 cycles.
DUT: lw_sw_copy unrolled: 8 cycles/iteration.
DUT: Expected: 8 cycles.
DUT: lw_sw_copy_loop: dest: 0x12040190, src: 0x12044190, 14 cycles/iteration.
DUT: Expected: 14.
DUT: Test Successful.
SIM: Test passed.
```

## Ibex Performance Test on Arty A7

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Build the project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/ibex_perf_test
make ibex_perf_test_bit
```

Download the generated bitstream file to the Arty A7:

```
make ibex_perf_test_load
```

In the terminal emulator, you should see the same output as in the Verilator test build above.

### The Timer, UART, and GPIO Interrupt Test on Verilator

The [Timer, UART, and GPIO Interrupt Test]() includes a timer interrupt test implementing the IRQ jitter removal mechanism.

Build the *timer_uart_gpio_irqs* gateware project:

```
cd build/sim-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_sim
```

Execute the generated Verilator model:

```
./Vmodel
IM: DUT: Enabling Ibex IRQs
SIM: DUT: Setting timer...
SIM: DUT: Timer IRQ expected at: 93733
SIM: DUT: Timer ISR after IRQ jitter removal started at: 93733
SIM: DUT: Timer IRQ expected at: 449645
SIM: DUT: Timer ISR after IRQ jitter removal started at: 449645
SIM: DUT: Timer Test Successful.
SIM: SIM: Timer test successful. Time = 3914732
...
```

### The Timer, UART, and GPIO Interrupt Test on FPGA

Connect a terminal emulator to Arty's USB serial port. **Settings: 115200 8N1**.

Build the *timer_uart_gpio_irqs* gateware project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_bit
```

Download the generated bitstream file to the Arty A7:

```
make timer_uart_gpio_irqs_load
```

Follow the prompts on the serial port terminal. Push the Arty's buttons or enter characters into the terminal when prompted.

### Peeking words with the DFX test on FPGA

The *peekw* CLI command in the [DFX test program]() measures the instruction cycle count of *peekw* load word (*lw*) transaction. This can be used to measure how long it takes to read a word from specific slaves (IMEM, UART...).

Hook up the MicroSD PMOD as described [here](https://boxlambda.readthedocs.io/en/latest/pmods/#microsd-pmod) and insert a FAT formatted SD card.

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
The tweaking of the BoxLambda SoC gateware to meet the requirement is now complete. The SoC a simple, low latency system with predictable instruction cycle counts. This is a milestone! I'm looking forward to the next phase of the project: Software. BoxLambda needs a run-time environment. What will it be? BASIC? Lua? Forth? Lisp? Cross Compilation? Wait and See...

