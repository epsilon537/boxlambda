---
hide:
  - toc
---

## Ibex RISC-V Interrupt Handling

### Vectored Mode

Ibex handles interrupts in *Vectored Mode*. Each interrupt has a separate entry point in a vector table. When an interrupt occurs, the CPU jumps to the address calculated by multiplying the `IRQ_ID` by four and adding it to the vector table base address. The vector table base address is specified in the `mtvec` CSR. In BoxLambda, I'm leaving it at 0, so the interrupt entry point address is simply `IRQ_ID*4`.

### Vectors.S Weak Bindings

The interrupt entry points are all defined in the bootstrap component's [vector.S](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/vectors.S) module. Each entry point is 4 bytes wide, so there's just enough space for an instruction to jump to the actual interrupt service routine of the interrupt in question. This creates a small problem: If you insert into the vector table a straightforward call to your application-specific interrupt service routine, you end up with an inverted dependency. You don't want the lowest layer platform code to depend directly on the higher layer application code. To get around that issue, I defined *weak bindings* for all the interrupt service routines inside `vectors.S`:

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

As you can see, the weak bindings jump to `_exc_handler`, and the default `_exc_handler` jumps to itself. The idea is that these default weak bindings never get invoked and that they are replaced with actual interrupt service routine implementations in higher-layer code. I put the C language declarations of the interrupt service routines in [interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h):

```
void __attribute__((naked)) _vera_irq_handler(void);
void __attribute__((naked)) _vs_0_irq_handler(void);
void __attribute__((naked)) _sdspi_irq_handler(void);
void __attribute__((naked)) _gpio_irq_handler(void);
void __attribute__((naked)) _usb_hid_1_irq_handler(void);
void __attribute__((naked)) _usb_hid_0_irq_handler(void);
void __attribute__((naked)) _i2c_irq_handler(void);
void __attribute__((naked)) _uart_irq_handler(void);
void __attribute__((naked)) _dfx_irq_handler(void);
```

#### The *naked* attribute

The CPU switches to an [interrupt register bank](components_ibex.md#interrupt-shadow-registers) when entering interrupt mode. This means that the ISR no longer needs to save and restore the registers it uses. The regular ISR prologue and epilogue code (saving and restoring registers) can be skipped. This is done by declaring the ISR function with the GCC `naked` attribute.

Because the `naked` attribute skips all epilogue code, we have to insert the `mret` instruction ourselves in the ISR. 

For example:

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

The corresponding disassembly:

```
0000070c <_timer_irq_handler>:
     70c:       0b8000ef                jal     7c4 <mtimer_disable_raw_time_cmp>
     710:       00100713                li      a4,1
     714:       4ae1a223                sw      a4,1188(gp) # 536c <timer_irq_fired>
     718:       30200073                mret
```

The interrupt shadow register feature, combined with `naked` ISRs, results in very low interrupt overhead. For the timer interrupt example above, the ISR timing looks like this:

[![Interrupt Overhead.](assets/irq_overhead_after.png)](assets/irq_overhead_after.png)

*Interrupt Overhead with interrupt shadow registers and naked ISR.*

### Enabling Interrupts

To receive interrupts, you need to enable the global CPU interrupt, `mstatus.MIE`, as well as the specific Machine Interrupts you want to receive by setting their bits in the MIE CSR.

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

`Interrupts.h` provides the interrupt API:

- It defines all IRQ_IDs.
- It declares, but does not define (that's up to application code), all interrupt service routines.
- It provides functions to enable/disable global and machine interrupts.

[https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h)

