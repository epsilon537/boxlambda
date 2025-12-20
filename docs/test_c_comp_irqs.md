# Ibex RISC-V Interrupt Handling in Test C Components

API: [https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/interrupts/interrupts.h)

This section is specific to interrupt handling in Gateware test builds. See section [Interrupt Handling Software](forth_irqs.md) for general notes about the interrupt handling software.

`Interrupts.h` contains the interrupt API:

- It defines all IRQ_IDs.
- It provides functions to enable/disable global and machine interrupts.
- It declares, but does not define (that's up to application code), all interrupt service routines:

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

### The *naked* attribute

The CPU switches to an [interrupt register bank](components_ibex.md#interrupt-shadow-registers) when entering interrupt mode. This means that the ISR doesn't need to save and restore the registers it uses. The regular ISR prologue and epilogue code (saving and restoring registers) can be skipped. This is done by declaring the ISR function with the GCC `naked` attribute.

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

## Enabling Interrupts

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


