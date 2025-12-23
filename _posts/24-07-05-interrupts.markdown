---
layout: post
title: 'Hardware and Timer Interrupts.'
comments: true
mathjax: yes
---

*Updated 23 December 2025:*
- *Removed references to PicoRV DMA which is no longer included in the SoC.*
- *Removed reference to 'On WSL' documentation.*

BoxLambda is inching closer to gateware-completeness. I added interrupt support so the various components of the BoxLambda SoC can report specific events back to the CPU and/or indicate that software intervention may be required. For better or worse, I've spent a good portion of this post wrestling with the terms *Edge-Triggered* and *Level-Sensitive* interrupt.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_IRQ_Focus.png)

This is a summary of the current state of affairs for BoxLambda. We have:
- An Ibex RISC-V core, Wishbone Interconnect, timer, two 8-pin GPIO ports, UART.
- Harvard Architecture internal memory.
- DDR3 external memory access through the Litex Memory Controller.
- OpenOCD-based Debug Access, both on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- SPI Flash Controller.
- USB HID Keyboard and Mouse support.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- Test builds running on Arty-A7-35T, Arty-A7-100T, Verilator, and CocoTB.
- A Linux CMake and Bender-based Software and Gateware build system with support for automated testing and post-implementation memory updates.

The Interrupt Handling Protocol - Condition- vs. Event-Based Interrupts
-----------------------------------------------------------------------
A core, e.g. a UART, may raise an interrupt request (IRQ) when it detects an event or condition that may be of interest to software running on the CPU. In the case of a UART, the condition might be *Rx-FIFO-Not-Empty*, or the event might be *Character-Enters-Empty-Rx-FIFO*. Interrupt triggers come in two flavors:
- **Condition-Based Interrupts**: A core that implements condition-based interrupts will assert its interrupt request (IRQ) line when a certain condition is met, for as long as that condition is met. The CPU's interrupt handler routine has to remove the condition generating the interrupt before it can return from interrupt. E.g. A UART core that implements condition-based interrupts will assert its IRQ line when the RX FIFO is not empty. The IRQ line remains asserted as long as the RX FIFO is not empty. This means that the interrupt handler routine servicing this interrupt will have to read all the data from the RX FIFO or disable the corresponding UART interrupt before it can return from interrupt. If the interrupt handler routine would return before emptying the FIFO or disabling the interrupt source, the CPU would immediately get re-interrupted.

![Condition Based Interrupts](../assets/condition_based_irq.png)

*Condition-Based Interrupt Example.*

Note that in the condition-based interrupt example, there is no explicit interrupt acknowledgment. The IRQ is de-asserted when the FIFO is empty.

- **Event-Based Interrupts**: A core implementing event-based interrupts will assert its IRQ line when a specific event happens. The IRQ line remains asserted until the CPU clears/acknowledges the event by writing to an interrupt register inside the core. E.g. a UART core that implements event-based interrupts will assert its IRQ line when the RX FIFO goes from an empty to a non-empty state. The IRQ line remains asserted until the CPU writes a 1 into the *Rx_Data_Avl* bit position of the UART core's Interrupt Status Register.

![Event-Based Interrupts](../assets/event_based_irq.png)

*Event-Based Interrupt Example.*

In the event-based interrupt example, the interrupt is explicitly acknowledged by a write to the ISR register. Also, the interrupt handler routine *may* read the received character from the Rx FIFO, but this is not required. Retrieving the character from the Rx FIFO may be deferred to a later time when the CPU is no longer in interrupted mode. In the event-based protocol, interrupt handling is decoupled from handling the condition that triggered the interrupt.

WBUART and SDSPI Interrupt Protocol Modification
================================================
The original **wbuart** and **sdspi** cores use condition-based interrupts. I find it easier to work with the event-based protocol, however. I modified those cores to generate event-based interrupts.

In the case of wbuart, I added an Interrupt Enable (IEN) register and an Interrupt Status (ISR) register. The ISR bits get set when specific events happen:
- **ISR[0]**: The UART Rx FIFO goes from an empty to a non-empty state
- **ISR[1]**: The UART Rx FIFO goes from less-than-half-full to half-full.
- **ISR[2]**: The UART Tx FIFO goes from half-full to less than half-full.
- **ISR[3]**: The UART Tx FIFO goes from non-empty to empty.

ISR bits get cleared by the CPU writing a 1 into the bit position of the event it wants to acknowledge. When any IEN-enabled ISR bits are set, the wbuart core's IRQ line is asserted.

The modification of the sdspi is almost identical. Here, the events are:
- **ISR[0]**: The device transitioned from a *busy* state to an *idle* state.
- **ISR[1]**: The removal of an SD card is detected.

Most other cores on the BoxLambda SoC already implement the event-based protocol, the exception being **wb_timer**, which I'll discuss separately below.

Edge Triggered Interrupt Controllers connected to a Level-Sensitive CPU
=======================================================================
The terms *Condition-based* and *Event-based* interrupt are just terms I came up with for this discussion. Some would say that the generally accepted term for what I'm calling *Condition-Based* is *Level-Triggered*, while the official name of an *Event-Based* interrupt is *Edge-Triggered* interrupt. Depending on your perspective, there's some truth to that. Different people have different takes on these terms. You can find the Wikipedia definition of edge- and level-triggered [here](https://en.wikipedia.org/wiki/Interrupt#Triggering_methods).

The Ibex CPU is level-sensitive, no question about that. It responds to the levels of the incoming IRQ lines. It does not register edges or pulses.

However, you could argue that when I modified the UART core to generate event-based interrupts, I added a lightweight, **edge-triggered interrupt controller** to that core.

Here's the original UART core:

![UART Level Sensitive Interrupts](../assets/uart_level_sensitive_irq_orig.png)

*The UART Core before modification: Level Sensitivity from source to CPU.*

Here's the UART core after modification:

![UART Edge Triggered Interrupts](../assets/uart_edge_triggered_irqs_new.png)

*The UART Core after modification: Level Sensitive CPU, 'Mini' Edge-Triggered Interrupt Controller inside UART Core.*

From this perspective, the overall interrupt architecture of the BoxLambda SoC is that of a level-sensitive CPU connected to a bunch of small, edge-triggered interrupt controllers (*MiniEdgeICs*) inside the components cores.

![Mini Edge ICs](../assets/miniEdgeICs.png)

*BoxLambda SoC with Mini, Edge-Triggered Interrupt Controllers inside the component cores.*

The Timer Module
----------------
**Wb_timer** is a basic timer module capable of generating interrupts based on the RISC-V **Machine Timer** Registers. The RISC-V spec defines two Machine Time Registers:
- **Mtime** is a 64-bit real-time counter. The RISC-V spec doesn't specify the frequency. In BoxLambda it's running at 50MHz, the system clock frequency. Mtime is a Wishbone-accessible register.
- **Mtimecmp** is a 64-bit timer compare register. When *mtime* is greater than or equal to *mtimecmp*, a timer interrupt is posted. The interrupt is cleared by writing to the mtimecmp register and setting it to a value greater than *mtime*. Mtimecmp is a Wishbone-accessible register.

The wb_timer module doesn't follow the ISR/IEN *MiniEdgeIC* pattern. It would be easy enough to add an IEN and ISR register, but doing that would break compliance with the RISCV spec.

The RISC-V Machine Timer Registers specification can be found in section 3.1.15 of the RISC-V Privileged Specification:

[https://riscv.org/wp-content/uploads/2017/05/riscv-privileged-v1.10.pdf](https://riscv.org/wp-content/uploads/2017/05/riscv-privileged-v1.10.pdf)

This is the timer module software API:

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/timer/timer.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/timer/timer.h)

And this is the timer core's verilog code:

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/wb_timer/rtl/wb_timer.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/wb_timer/rtl/wb_timer.sv)
[https://github.com/epsilon537/ibex/blob/boxlambda/shared/rtl/timer.sv](https://github.com/epsilon537/ibex/blob/boxlambda/shared/rtl/timer.sv)

Ibex RISC-V Interrupt Assignments
---------------------------------

![BoxLambda Interrupts](../assets/irq_diagram.png)

*BoxLambda Interrupt Diagram.*

The RISC-V spec defines an elaborate interrupt architecture, with lots of options and flexibility for different types of implementations. An easier starting point for BoxLambda is the specific interrupt handling implementation chosen for the Ibex RISC-V processor, documented here:

[https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html)

Briefly, the Ibex core has:
- 15 ports for **Fast**, local interrupts. The BoxLambda SoC components that can post IRQs (not all of them do) are connected to these fast interrupts.
- 1 port for a **Timer** interrupt. The *wb_timer* module is connected to this interrupt line.
- An **External Interrupt** port to connect a so-called *Platform-Level Interrupt Controller*. I'm not going to use this.
- A **Non-Maskable Interrupt** (NMI) port, which I'll also leave unconnected until I find a good use for it.

RISC-V defines 32 IRQ IDs. Ibex maps the timer interrupt to IRQ ID 7 and the fast interrupts to IRQ IDs 16 to 31.

This table lists the BoxLambda interrupts and the events they report:

| IRQ_ID | IRQ Name                                | Events |
|--------|-----------------------------------------|--------|
| 30     | RM_2 interrupt (Default: not assigned)  |        |
| 29     | RM_1 interrupt (Default: VERA IRQ)      | Vsync, Line IRQ, Sprite Collision |
| 28     | RM_0 interrupt (Default: not assigned)  |        |
| 27     | PICORV DMAC IRQ                         | Programmable |
| 26     | SDSPI IRQ                               | Device Busy->Idle, Card Removed |
| 25     | GPIO                                    | Rising or Falling Edge on input pin |
| 24     | usb_hid_1 IRQ                           | USB report received, LED set confirmation |
| 23     | usb_hid_0 IRQ                           | USB report received, LED set confirmation |
| 22     | I2C IRQ                                 | Device Busy->Idle |
| 21     | UART                                    | Rx FIFO not empty, Rx FIFO half full, Tx FIFO half empty, Tx FIFO empty |
| 20     | not assigned                            |     |
| 19     | not assigned                            |     |
| 18     | not assigned                            |     |
| 17     | DFX Controller IRQ                      | TBD |
| 16     | ICAP IRQ                                | TBD |
|  7     | Timer IRQ                               | timer counter >= timer compare value |

Ibex RISC-V Interrupt Handling
------------------------------

CSRs
====
RISC-V CSRs, Control and Status Registers, are special registers through which the programmer can configure the CPU and query its status. CSR registers live in a separate address space, accessed through special instructions such as:
- *csrr a0, mstatus*: read mstatus CSR contents into register a0
- *csrw mtvec, a1*: write register a1 contents into the mtvec CSR.

Some CSRs have several subfields. The CSRs and subfields of interest for interrupt handling are listed below:

| CSR(.subfield) | Bit Position(s) | Description |
|----------------|-----------------|-------------|
| **mstatus.MIE** | 3 | Machine Interrupt Enable: The CPU's global Interrupt Enable bit.|
| **mcause.INTERRUPT** | 31 | Set if the interrupt was caused by a regular interrupt (1) or an exception (0).|
| **mcause.EXCCODE** | 4:0 | If mcause.INTERRUPT is set, mcause.EXCCODE contains the IRQ_ID.|
| **mtvec.BASE** | 30:0 | The Interrupt Vector Table Base Address.base address aligned to 256 bytes, i.e. mtvec[7:2] is always set to 6’b0.|
| **mtvec.MODE** | 1:0 | On Ibex this is fixed at 2'b01 indicating vectored mode. Direct Mode is not supported.|
| **mie** | 31:0 | Machine Interrupt Enable. Each bit position corresponds to an IRQ_ID to enable(1)/disable(0).|
| **mip** | 31:0 | Machine Interrupt Pending. Each bit position corresponds to an IRQ_ID. A bit set to 1 indicates that IRQ_ID has a pending interrupt.|
| **mepc** | 31:0 | Machine Exception PC. The current Program Counter when the CPU got interrupted.|

For a complete list of CSRs, see the [Ibex Controls and Status Registers](https://ibex-core.readthedocs.io/en/latest/03_reference/cs_registers.html#cs-registers) page.

To be able to access the CSRs easily from C/C++ I'm using Five Embeddev's *riscv-csr-access* library:

[https://github.com/five-embeddev/riscv-csr-access](https://github.com/five-embeddev/riscv-csr-access)

The library consists of a single .h file, which I copied into BoxLambda's *riscv* software component directory:

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/riscv-csr.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/riscv/riscv-csr.h)

Vectored Mode
=============
Ibex handles interrupts in *Vectored Mode*. Each interrupt has a separate entry point in a vector table. When an interrupt occurs, the CPU jumps to the address calculated by multiplying the IRQ_ID by four and adding it to the vector table base address. The vector table base address is specified in the *mtvec* CSR. In BoxLambda, I'm leaving it at 0, so the interrupt entry point address is simply IRQ_ID*4.

\\[
\textrm{BoxLambda Interrupt Entry Point Address} = \textrm{IRQ_ID}\times4
\\]

Inverted Dependencies and Weak Bindings
=======================================
The interrupt entry points are all defined in the bootstrap component's [vector.S](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/vectors.S) module. Each entry point is 4 bytes wide so there's just enough space for an instruction to jump to the actual interrupt service routine of the interrupt in question. This creates a small problem: If you insert into the vector table a straightforward call to your application-specific interrupt service routine, you end up with an inverted dependency. You don't want the lowest layer platform code to depend directly on the higher layer application code. To get around that issue, I defined *weak bindings* for all the interrupts service routines inside vectors.S:

```
// Weak bindings for the fast IRQs. These will be overridden in the
// application code requiring interrupt handling for a given source.
.globl _icap_irq_handler
.weak _icap_irq_handler
_icap_irq_handler:
.globl _dfx_irq_handler
.weak _dfx_irq_handler
_dfx_irq_handler:
.globl _uart_irq_handler
.weak _uart_irq_handler
_uart_irq_handler:
.globl _i2c_irq_handler
.weak _i2c_irq_handler
_i2c_irq_handler:
.globl _usb_hid_0_irq_handler
.weak _usb_hid_0_irq_handler
_usb_hid_0_irq_handler:
.globl _usb_hid_1_irq_handler
.weak _usb_hid_1_irq_handler
_usb_hid_1_irq_handler:
.globl _gpio_irq_handler
.weak _gpio_irq_handler
_gpio_irq_handler:
.globl _sdspi_irq_handler
.weak _sdspi_irq_handler
_sdspi_irq_handler:
.globl _dmac_irq_handler
.weak _dmac_irq_handler
_dmac_irq_handler:
.globl _rm_0_irq_handler
.weak _rm_0_irq_handler
_rm_0_irq_handler:
.globl _rm_1_irq_handler
.weak _rm_1_irq_handler
_rm_1_irq_handler:
.globl _rm_2_irq_handler
.weak _rm_2_irq_handler
_rm_2_irq_handler:
.globl _timer_irq_handler
.weak _timer_irq_handler
_timer_irq_handler:
  jal x0, _exc_handler //If the IRQ handler does not get overridden and the IRQ fires, jump to the exception handler.
.weak _exc_handler
_exc_handler:          //_exc_handler is overridden in the interrupts SW module.
  jal x0, _exc_handler
```

As you can see, the weak bindings jump to *_exc_handler*, and the default _exc_handler jumps to itself. The idea is that these default weak bindings never get invoked and that they get overruled with actual interrupt service routine implementations in higher layer code. I put the C language declarations of the interrupt service routines in [interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h):

```
void _rm_2_irq_handler(void) __attribute__((interrupt("machine")));
void _rm_1_irq_handler(void) __attribute__((interrupt("machine")));
void _rm_0_irq_handler(void) __attribute__((interrupt("machine")));
void _dmac_irq_handler(void) __attribute__((interrupt("machine")));
void _sdspi_irq_handler(void) __attribute__((interrupt("machine")));
void _gpio_irq_handler(void) __attribute__((interrupt("machine")));
void _usb_hid_1_irq_handler(void) __attribute__((interrupt("machine")));
void _usb_hid_0_irq_handler(void) __attribute__((interrupt("machine")));
void _i2c_irq_handler(void) __attribute__((interrupt("machine")));
void _uart_irq_handler(void) __attribute__((interrupt("machine")));
void _dfx_irq_handler(void) __attribute__((interrupt("machine")));
void _icap_irq_handler(void) __attribute__((interrupt("machine")));

void _timer_irq_handler(void) __attribute__((interrupt("machine")));
```

The Interrupt("machine") Attribute
==================================

Regarding the *interrupt("machine")* attribute in the function declarations above, interrupt service routines require a special entry and exit code sequence. An interrupt service routine needs to save and restore all CPU registers it's modifying to ensure the interrupted code can continue normally when the CPU returns from the interrupt. Also, instead of a regular return, an interrupt service routine should execute a return-from-interrupt when done. These concepts can't be expressed in regular C language, but GCC comes to the resource with the *interrupt("machine")* attribute. This attribute ensures the function receives the proper prologue and epilogue code to make it behave as an interrupt service routine.

As an example of the generated code when using *interrupt("machine")*, here's a disassembly of the timer interrupt service routine I'm using in the interrupt test program:

```
95	void _timer_irq_handler(void) {
96	 //Stop the timer. If we don't stop it, or move into the future, the IRQ will keep on firing.
97	 mtimer_disable_raw_time_cmp();
98	 timer_irq_fired = 1;
99	}
100
101	int main(void) {
(gdb) disassemble _timer_irq_handler
Dump of assembler code for function _timer_irq_handler:
   0x000006d8 <+0>:	add	sp,sp,-64
   0x000006da <+2>:	sw	a4,28(sp)
   0x000006dc <+4>:	sw	a5,24(sp)
   0x000006de <+6>:	sw	ra,60(sp)
   0x000006e0 <+8>:	sw	t0,56(sp)
   0x000006e2 <+10>:	sw	t1,52(sp)
   0x000006e4 <+12>:	sw	t2,48(sp)
   0x000006e6 <+14>:	sw	a0,44(sp)
   0x000006e8 <+16>:	sw	a1,40(sp)
   0x000006ea <+18>:	sw	a2,36(sp)
   0x000006ec <+20>:	sw	a3,32(sp)
   0x000006ee <+22>:	sw	a6,20(sp)
   0x000006f0 <+24>:	sw	a7,16(sp)
   0x000006f2 <+26>:	sw	t3,12(sp)
   0x000006f4 <+28>:	sw	t4,8(sp)
   0x000006f6 <+30>:	sw	t5,4(sp)
   0x000006f8 <+32>:	sw	t6,0(sp)
   0x000006fa <+34>:	jal	0x832 <mtimer_disable_raw_time_cmp>
   0x000006fc <+36>:	li	a4,1
   0x000006fe <+38>:	sw	a4,608(gp)
   0x00000702 <+42>:	lw	ra,60(sp)
   0x00000704 <+44>:	lw	t0,56(sp)
   0x00000706 <+46>:	lw	t1,52(sp)
   0x00000708 <+48>:	lw	t2,48(sp)
   0x0000070a <+50>:	lw	a0,44(sp)
   0x0000070c <+52>:	lw	a1,40(sp)
   0x0000070e <+54>:	lw	a2,36(sp)
   0x00000710 <+56>:	lw	a3,32(sp)
   0x00000712 <+58>:	lw	a4,28(sp)
   0x00000714 <+60>:	lw	a5,24(sp)
   0x00000716 <+62>:	lw	a6,20(sp)
   0x00000718 <+64>:	lw	a7,16(sp)
   0x0000071a <+66>:	lw	t3,12(sp)
   0x0000071c <+68>:	lw	t4,8(sp)
   0x0000071e <+70>:	lw	t5,4(sp)
   0x00000720 <+72>:	lw	t6,0(sp)
   0x00000722 <+74>:	add	sp,sp,64
   0x00000724 <+76>:	mret
End of assembler dump.
(gdb) quit
```

Here's the GCC page about RISC-V function attributes:

[https://gcc.gnu.org/onlinedocs/gcc/RISC-V-Function-Attributes.html](https://gcc.gnu.org/onlinedocs/gcc/RISC-V-Function-Attributes.html)

Enabling Interrupts
===================
To receive interrupts, you need to enable the global CPU interrupt, *mstatus.MIE*, as well as the specific Machine Interrupts you want to receive by setting their bits in the MIE CSR.

[Interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h) defines functions to enable and disable global as well as specific machine interrupts:

```
/* Disable the global interrupt line at CPU level.*/
static inline void disable_global_irq(void) {
  CSR_CLR_BITS_IMM_MSTATUS(MSTATUS_MIE_BIT_MASK);
}

/*Enable the global interrupt line at CPU level.*/
static inline void enable_global_irq(void) {
  CSR_SET_BITS_IMM_MSTATUS(MSTATUS_MIE_BIT_MASK);
}

/* Enable a specific interrupt ID at CPU level. Note that for interrupts
 * to go through, the global interrupt line also needs to be enabled
 * (enable_global_irq).*/
static inline void enable_irq(unsigned irq_id) {
  csr_set_bits_mie(1<<irq_id);
}

/* Disable a specific interrupt ID at CPU level. */
static inline void disable_irq(unsigned irq_id) {
 csr_clr_bits_mie(1<<irq_id);
}

/* Disable all interrupt ids at CPU level.*/
static inline void disable_all_irqs(void) {
  csr_clr_bits_mie(~0u);
}
```

Nested Interrupts
=================
I haven't tested it, but RISC-V supports interrupt nesting. You can find more info here:

[https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html#nested-interrupt-exception-handling](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html#nested-interrupt-exception-handling)

The Interrupt API
-----------------
*Interrupts.h* provides the interrupt API:
- It defines all IRQ_IDs.
- It declares, but does not define (that's up to application code), all interrupt service routines.
- It provides functions to enable/disable global and machine interrupts.

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h)

Testing
-------
The VERA, SDSPI, and USB interrupts I tested by extending their respective system test cases to include validation of interrupt triggers.

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/vera_test/vera_test.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/vera_test/vera_test.c)

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/sdspi_test/sdtest.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/sdspi_test/sdtest.c)

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/usb_hid_sys_test/usb_hid_sys_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/usb_hid_sys_test/usb_hid_sys_test.cpp)

For testing Timer, GPIO, and UART interrupts, I created a separate system test case called **timer_uart_gpio_irqs**.

Some external interaction is required for GPIO and UART testing. On FPGA, follow the prompts on the serial port terminal ("Enter a character", "Push a button",...). On Verilator I want a fully automated test case, so the test bench pushes the simulated GPIO buttons and sends the UART characters when it detects the corresponding prompts in the serial port output.

This is the test program running on Ibex:

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/timer_uart_gpio_irqs/timer_uart_gpio_irqs.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/timer_uart_gpio_irqs/timer_uart_gpio_irqs.c)

And this is the verilator test bench code:

[https://github.com/epsilon537/boxlambda/blob/master/gw/projects/timer_uart_gpio_irqs/sim/sim_main.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/timer_uart_gpio_irqs/sim/sim_main.cpp)

Other Changes
-------------
The GPIO core I've been using in BoxLamdba until recently was a simple core that didn't support interrupt handling. I replaced it with this *Opencores* core:

[https://github.com/xfguo/gpio](https://github.com/xfguo/gpio)

![GPIO Core Block Diagram](../assets/gpio_core_diagram.jpg)

*GPIO Core Block Diagram.*

Each input on this GPIO core can be configured to generate an IRQ when a rising or falling edge is detected.

Here is the spec:

[https://github.com/xfguo/gpio/blob/master/doc/gpio_spec.pdf](https://github.com/xfguo/gpio/blob/master/doc/gpio_spec.pdf)

Try It Yourself
---------------

Setup
=====
1. Install the [Software Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
2. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
1. Switch to the **interrupts** tag:
```
git checkout interrupts
```
1. Set up the repository. This initializes the git submodules used and creates the default build trees:
```
./boxlambda_setup.sh
```

The Timer, UART, and GPIO Interrupt Test on Verilator
=====================================================
1. Build the *timer_uart_gpio_irqs* gateware project:
```
cd build/sim-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_sim_sw
```

1. Execute the generated Verilator model:
```
./Vmodel
SIM: DUT: Load PicoRV Program picorv_irq_in_out.
SIM: DUT: Taking PicoRV out of reset...
SIM: DUT: Enabling Ibex IRQs
SIM: DUT: Setting timer...
SIM: DUT: Timer irq fired.
SIM: DUT: Timer irq fired.
SIM: DUT: Timer Test Successful.
SIM: SIM: Timer test successful. Time = 3208396
SIM: DUT: Testing UART TX IRQs...
SIM: DUT: 0123456789
SIM: DUT: UART TX IRQ test successful.
SIM: SIM: UART TX IRQ test successful. Time = 4319436
SIM: DUT: Testing UART RX IRQs...
SIM: DUT: Please enter a character.
SIM: SIM: inserting uart character (1). Time = 5187436
SIM: DUT: UART RX IRQ received.
SIM: DUT: Received character: a
SIM: DUT: Please enter another character.
SIM: SIM: inserting uart character (2). Time = 6526280
SIM: DUT: UART RX IRQ received.
SIM: DUT: Received character: b
SIM: DUT: Please enter 8 characters. They will be echoed when all 8 characters are received.
SIM: SIM: inserting character sequence. Time = 8750484
SIM: SIM: finished inserting uart string (1).
SIM: DUT: UART RX FIFO IRQ received.
SIM: DUT: Received character: H
SIM: DUT: Received character: e
...
SIM: DUT: Received character: SIM: DUT: Please enter 8 characters again. They will be echoed when all 8 characters are received.
SIM: SIM: Inserting character sequence again. Time = 15870184
SIM: SIM: finished inserting uart string (2).
SIM: DUT: UART RX FIFO IRQ received.
SIM: DUT: Received character: H
SIM: DUT: Received character: e
...
SIM: DUT: Received character: SIM: DUT: UART RX IRQ Test Successful.
SIM: SIM: UART RX IRQ tests successful. Time = 21948348
SIM: DUT: Testing GPIO IRQs...
SIM: DUT: Push some buttons. The LEDS should track the button presses/releases.
SIM: SIM: Testing GPIO. Time = 23528108
SIM: SIM: LED 0 turned on.
SIM: SIM: LED 0 turned off.
SIM: SIM: LED 1 turned on.
SIM: SIM: LED 1 turned off.
SIM: SIM: GPIO test OK.
SIM: SIM: GPIO test successful. Time = 23536664
SIM: Test passed.
```


The Timer, UART, and GPIO Interrupt Test on FPGA
================================================
1. Connect a terminal program to Arty's USB serial port. **Settings: 115200 8N1**.
2. Build the *timer_uart_gpio_irqs* gateware project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
```
cd build/arty-a7-100/gw/projects/timer_uart_gpio_irqs
make timer_uart_gpio_irqs_bit_sw
```
3. Download the generated bitstream file to the Arty A7:
```
make timer_uart_gpio_irqs_load
```
4. Follow the prompts on the serial port terminal. Push the Arty's buttons or enter characters into the terminal when prompted.

Conclusion
----------
There's a lot of ambiguous terminology surrounding interrupts. Is an event-based interrupt that gets acknowledged by writing to the core's ISR register a level-sensitive or an edge-triggered interrupt? What is the difference between acknowledging and clearing an interrupt? Is *ISR* an *Interrupt Service Routine* or an *Interrupt Status Register*? Are re-entrant interrupts the same as nested interrupts? If a project uses these terms, it's necessary to figure out what the terms mean exactly in the context of that project. The meanings might be slightly different from what you expected.

References
----------
[Ibex Exceptions and Interrupts](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html#nested-interrupt-exception-handling).

[Ibex Control and Status Registers](https://ibex-core.readthedocs.io/en/latest/03_reference/cs_registers.html).

[The RISC-V Privileged Spec](https://riscv.org/wp-content/uploads/2017/05/riscv-privileged-v1.10.pdf).

[RISC-V Assembly Programming](https://riscv-programming.org/).

[GCC RISC-V Function Attributes](https://gcc.gnu.org/onlinedocs/gcc/RISC-V-Function-Attributes.html).

