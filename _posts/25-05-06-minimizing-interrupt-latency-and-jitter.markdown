---
layout: post
title: 'Minimizing Interrupt Latency and Jitter.'
comments: true
mathjax: yes
---

In this post, I'll explore ways to improve interrupt latency and jitter on the BoxLambda SoC.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_dual_bus_DFX.png)

BoxLambda's current features:
- Target FPGA: Arty-A7-100T.
- Ibex RISC-V core with machine timer and hardware interrupt support.
- Harvard architecture-based interconnect.
- Low-latency register and memory access across the SoC.
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
- Picolibc-based standard C environment.
- Test application suite covering all SoC components, running on both FPGA and Verilator.
- A Linux CMake and Bender-based Software and Gateware build system.

Interrupt Latency Before Adjustments
------------------------------------
Let's take a look at the waveform of a timer interrupt firing:

[![Waveform of a Timer Interrupt Firing.](../assets/timer_irq_waveform_before_changes.png)](../assets/timer_irq_waveform_before_changes.png)

*Waveform of a Timer Interrupt Firing.*

You can see the timer interrupt firing while the CPU is executing the instruction at address 0x02ec. Five clock cycles later, the CPU branches to the interrupt vector at 0x1c and two clock cycles after that, we're in the timer interrupt service routine (ISR). Six clock cycles of interrupt latency. Not too bad, you might say.

The real performance cost is yet to come, however. Take a look at the timer ISR code:

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

That's 38 instructions, not including the routine that disables the timer! Where does all that code come from? From the `interrupt("machine")` attribute in the ISR declaration:

```
void _timer_irq_handler(void) __attribute__((interrupt("machine")));
```

The CPU registers need to be saved before the ISR can make any register changes, and restored after the ISR has done its work. This simple timer ISR takes 101 clock cycles to execute. There are 42 clock cycles between the rising edge of the timer IRQ signal and the execution of the first non-prologue instruction in the ISR.

[![Interrupt Overhead before changes.](../assets/irq_overhead_before.png)](../assets/irq_overhead_before.png)

*Interrupt Overhead before changes.*

Interrupt Shadow Registers
--------------------------
To reduce the ISR prologue and epilogue overhead, I added an interrupt shadow register bank to the CPU's register file:

![The Ibex Register File including a Interrupt Shadow Register Bank](../assets/irq_shadow_registers.png)

The modified register file has two register banks:
- The *thread-level* bank named `mem`
- The *interrupt-level* bank named `irq_mem`.

Following the pattern of Ibex's `nmi_mode`, I added an `irq_mode` signal that tracks when the CPU enters and exits an ISR.
The `irq_mode` signal controls how register reads and writes are handled in the Register File:
- When the CPU is running at thread-level (i.e., not in IRQ mode), register writes are stored in both register banks, and register reads are fetched from the *mem* bank.
- When the CPU is running in IRQ mode, register writes are only stored in the `irq_mem` register bank, and register reads are fetched from the `irq_mem` bank.

With this design, the `irq_mem` bank is up-to-date when the CPU enters IRQ mode. Any register writes while in IRQ mode go to the `irq_mem` bank only and don't affect the thread-level `mem` bank. This means that no registers need to be saved or restored when switching between thread-level mode and IRQ mode.

**Caveat**: `irq_mode` is just a 1-bit signal. It can't track nested interrupts. This limitation is acceptable for BoxLambda.

The *naked* attribute
=====================
How do we tell the compiler that it can skip the prologue and epilogue? Interestingly, the GCC compiler has a `naked` attribute that does exactly this.

```
void __attribute__((naked)) _timer_irq_handler(void);
```

Because the `naked` attribute skips all epilogue code, we have to insert the `mret` instruction ourselves in the ISR:

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

With a *naked* `_timer_irq_handler`, the prologue takes 2 clock cycles (the branch from the interrupt vector to the `timer_irq_handler` and the epilogue takes 5 clock cycles (the `mret` instruction).

The Gain
========
After implementing these changes, it takes 27 clock cycles to execute the ISR. There are 8 clock cycles between the rising edge of the timer IRQ signal and the execution of the first timer ISR specific instruction.

| Timer ISR | Without Interrupt Shadow Registers | With Interrupt Shadow Registers |
|-----------|------------------------------|---------------------------|
| *Real* Interrupt Latency | 43 clock cycles | 8 clock cycles |
| ISR Execution Time | 95 clock cycles | 27 clock cycles |
| ISR Prologue and Epilogue overhead | 75 clock cycles| 7 clock cycles |

[![Interrupt Overhead after changes.](../assets/irq_overhead_after.png)](../assets/irq_overhead_after.png)

*Interrupt Overhead after changes.*

Interrupt Jitter
----------------
Claiming six clock cycles of interrupt latency is not entirely accurate. The actual latency depends on the instruction being executed when the interrupt occurs. The CPU has to complete that instruction before it can jump to the interrupt vector. This constraint causes some interrupt latency jitter/variation. For asynchronous events, such as key presses, that won't matter. However, for critically timed events, there may be cases where you want to execute a sequence of instructions *exactly* at a given time, without any jitter.

To accommodate such use cases, I added an `MTIMEBLK` register to the Timer core. A write operation to this register blocks the CPU until the lower 8 bits of the `MTIME` register match the written value. This mechanism can be used to absorb Timer interrupt jitter, as shown in the diagram below:

![Absorbing Timer Interrupt Jitter with MTIMEBLK](../assets/mtimeblk_mechanism.png)

*Absorbing Timer Interrupt Jitter with MTIMEBLK.*

You program a Timer interrupt to fire a few clock cycles before the desired time. Then, inside the timer ISR, you write to `MTIMEBLK` to block until the desired time is reached exactly.

The [timer_uart_gpio_irqs](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/timer_uart_gpio_irqs/timer_uart_gpio_irqs.c) test implements this mechanism:

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

This mechanism can also be used to remove jitter from periodic interrupts, such as VGA line interrupts.

Other Changes
-------------
See the [CHANGELOG](https://github.com/epsilon537/boxlambda/blob/master/CHANGELOG.md).

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

The [Ibex Performance Test](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ibex_perf_test/ibex_perf_test.c) now includes an interrupt latency measurement test.

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

The [Timer, UART, and GPIO Interrupt Test](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/timer_uart_gpio_irqs/timer_uart_gpio_irqs.c) includes a timer interrupt test implementing the interrupt jitter removal mechanism.

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

Conclusion
----------
The tweaking of the BoxLambda SoC gateware to meet the requirements is now complete. The SoC is a simple, low-latency system with predictable behavior down to the clock cycle. **This is a milestone!** 

![Celebrate.](../assets/celebrate.png)

In the next phase of the project, the focus will shift to software. BoxLambda needs a self-contained runtime environment. One of the key questions I would like to answer is: How do you develop software for a resource-constrained computer on that resource-constrained computer? I can't wait to dive in!

