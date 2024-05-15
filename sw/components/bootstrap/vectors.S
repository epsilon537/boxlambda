  .section .vectors, "ax"
  .option norvc;

  // all external interrupts are handled by the same callback
  .org 0x00
  .rept 31
  nop
  .endr
  jal x0, _default_exc_handler

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
  jal x0, _reset_handler

  // illegal instruction exception
  .org 0x84
  jal x0, _default_exc_handler

  // ecall handler
  .org 0x88
  jal x0, _default_exc_handler

_default_exc_handler:
  jal x0, _default_exc_handler

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

