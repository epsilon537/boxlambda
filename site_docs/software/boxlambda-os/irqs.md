# Interrupt Handling

See [Base Platform Interrupt Handling](../base-platform/bootstrap/irqs.md).

## Words

[interrupt-common.s](https://github.com/epsilon537/boxlambda/blob/v0.5.0/sw/components/forth/interrupt-common.s)

`reset ( -- )`

- Reset at the hardware level.

`dint ( -- )`

- Disables interrupts.

`eint ( -- )`

- Enables interrupts.

`eint? ( -- )`

- Are interrupts enabled ?

`wfi ( -- )`

- Wait For Interrupt.

`nop ( -- )`

- No Operation. Hook for unused handlers.

`unhandled ( -- )`

- Message for unhandled interrupts.

`fault ( -- a-addr )`

- For all faults, if available.

`mcause ( -- mcause)`

- Which interrupt?

`mepc ( -- mepc )`

- Where did it occur?

`mie ( -- mie )`

- Get Machine Interrupt Enable.

`mip ( -- mip )`

- Get Machine Interrupt Pending.

`mie ( mie -- )`

- Set Machine Interrupt Enable.

`mip ( mip -- )`

- Set Machine Interrupt Pending.

`dfx ( -- a-addr )`

- DFX irq hook.

`uart ( -- a-addr )`

- uart irq hook.

`i2c ( -- a-addr )`

- i2c irq hook.

`usb_hid_0 ( -- a-addr )`

- usb_hid_0 irq hook.

`usb_hid_1 ( -- a-addr )`

- usb_hid_1 irq hook.

`gpio ( -- a-addr )`

- gpio irq hook.

`sdspi ( -- a-addr )`

- sdspi irq hook.

`vs0 ( -- a-addr )`

- vs0 irq hook.

`vera ( -- a-addr )`

- vera irq hook.

`timer ( -- a-addr )`

- timer irq hook.

`irq ( -- id )`

- VERA irq id.

`irq ( -- id )`

- VS0 irq id.

`irq ( -- id )`

- DFX irq id.

`irq ( -- id )`

- SDSPI irq id.

`irq ( -- id )`

- USB-HID-1 irq id.

`irq ( -- id )`

- USB-HID-0 irq id.

`irq ( -- id )`

- I2C irq id.

`irq ( -- id )`

- UART irq id.

Example:

```
\ IRQ handler routine
: mtime-irq-handle
  ." IRQ mip : " mip hex. cr
  ." IRQ mcause : " mcause hex. cr
  -1 s>d mtimecmp64! \ Disable timer IRQ
  ." MTIMER IRQ received." cr ;

dint
0 mie!
\ Install timer interrupt handler hook
' mtime-irq-handle irq-timer !
\ Disable timer

-1 s>d mtimecmp64!
\ Enable timer IRQ
1 irq-id-timer lshift mie! eint
\ Request timer IRQ 3 seconds from now.
50000000 3 * set-raw-time-cmp
```

