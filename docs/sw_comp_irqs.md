---
hide:
  - toc
---

## Ibex RISC-V Interrupt Handling

### Vectored Mode

Ibex handles interrupts in *Vectored Mode*. Each interrupt has a separate entry point in a vector table. When an interrupt occurs, the CPU jumps to the address calculated by multiplying the IRQ_ID by four and adding it to the vector table base address. The vector table base address is specified in the *mtvec* CSR. In BoxLambda, I'm leaving it at 0, so the interrupt entry point address is simply IRQ_ID*4.

### Vectors.S Weak Bindings

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
_exc_handler:          //_exc_handler is overridden in the interrupt SW module.
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

#### The Interrupt("machine") Attribute

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

### Enabling Interrupts

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

### Nested Interrupts

I haven't tested it, but RISC-V supports interrupt nesting. You can find more info here:

[https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html#nested-interrupt-exception-handling](https://ibex-core.readthedocs.io/en/latest/03_reference/exception_interrupts.html#nested-interrupt-exception-handling)

### Interrupt API

*Interrupts.h* provides the interrupt API:

- It defines all IRQ_IDs.
- It declares, but does not define (that's up to application code), all interrupt service routines.
- It provides functions to enable/disable global and machine interrupts.

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h)

