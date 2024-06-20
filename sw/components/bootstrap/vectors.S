  .section .vectors, "ax"
  .option norvc;

  .org 0x00
  .rept 7
  jal x0, _exc_handler
  .endr
  jal x0, _timer_irq_handler
  jal x0, _exc_handler /*IRQ_ID 8*/
  jal x0, _exc_handler /*IRQ_ID 9*/
  jal x0, _exc_handler /*IRQ_ID 10*/
  jal x0, _exc_handler /*IRQ_ID 11*/
  jal x0, _exc_handler /*IRQ_ID 12*/
  jal x0, _exc_handler /*IRQ_ID 13*/
  jal x0, _exc_handler /*IRQ_ID 14*/
  jal x0, _exc_handler /*IRQ_ID 15*/
  jal x0, _icap_irq_handler
  jal x0, _dfx_irq_handler
  jal x0, _exc_handler /*branch to exception handler for the not-assigned fast IRQs.*/
  jal x0, _exc_handler
  jal x0, _exc_handler
  jal x0, _uart_irq_handler
  jal x0, _i2c_irq_handler
  jal x0, _usb_hid_0_irq_handler
  jal x0, _usb_hid_1_irq_handler
  jal x0, _gpio_irq_handler
  jal x0, _sdspi_irq_handler
  jal x0, _dmac_irq_handler
  jal x0, _rm_0_irq_handler
  jal x0, _rm_1_irq_handler
  jal x0, _rm_2_irq_handler
  jal x0, _exc_handler /*NMI*/

  // reset vector. The first instruction executed after reset (and power-on)
  // is here. This is also the case when we 'boot from flash'. We always
  // start from this vector, but may jump to flash memory at the end of the
  // _reset_handler function.
  // In case of boot-from-flash builds, the CMEM image included in the bitstream
  // contains just this vector.S module, nothing else.
  // This CMEM image is created in the sw/projects/cmem_to_flash_vector project.
  // In case of boot-from-cmem builds, the CMEM image included in the bitstream
  // contains the actual SW image, which starts with this vector.S module.
  .org 0x80
_reset_handler:
  // set all registers to zero
  mv  x1, x0
  mv  x2, x1
  mv  x3, x1
  mv  x4, x1
  mv  x5, x1
  mv  x6, x1
  mv  x7, x1
  mv  x8, x1
  mv  x9, x1
  mv x10, x1
  mv x11, x1
  mv x12, x1
  mv x13, x1
  mv x14, x1
  mv x15, x1
  mv x16, x1
  mv x17, x1
  mv x18, x1
  mv x19, x1
  mv x20, x1
  mv x21, x1
  mv x22, x1
  mv x23, x1
  mv x24, x1
  mv x25, x1
  mv x26, x1
  mv x27, x1
  mv x28, x1
  mv x29, x1
  mv x30, x1
  mv x31, x1
  // Tail call pseudo instruction. _reset_handler is always located
  // in cmem, while _start might be located in flash memory (in case of
  // a SW image linked to boot from flash), in which
  // case we have to make a non-local jump.
  tail _start

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
_exc_handler:          //_exc_handler is overridden in the bootstrap SW module.
  jal x0, _exc_handler
