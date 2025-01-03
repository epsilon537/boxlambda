#ifndef INTERRUPTS_H
#define INTERRUPTS_H

#include "riscv-csr.h"

#ifdef __cplusplus
extern "C" {
#endif
/* The following interrupt handler functions are weakly bound
 * to a do-nothing (mret) function.
 * To implement an interrupt handler for a specific IRQ, just
 * provide a definition for the corresponding function below.
 * It'll overrule the weak binding.
 */
void _vera_irq_handler(void) __attribute__((interrupt("machine")));
void _vs_0_irq_handler(void) __attribute__((interrupt("machine")));
void _dmac_irq_handler(void) __attribute__((interrupt("machine")));
void _sdspi_irq_handler(void) __attribute__((interrupt("machine")));
void _gpio_irq_handler(void) __attribute__((interrupt("machine")));
void _usb_hid_1_irq_handler(void) __attribute__((interrupt("machine")));
void _usb_hid_0_irq_handler(void) __attribute__((interrupt("machine")));
void _i2c_irq_handler(void) __attribute__((interrupt("machine")));
void _uart_irq_handler(void) __attribute__((interrupt("machine")));
void _dfx_irq_handler(void) __attribute__((interrupt("machine")));

void _timer_irq_handler(void) __attribute__((interrupt("machine")));

/* Exception Handler */
void _exc_handler(void) __attribute__((interrupt("machine")));

/* Disable the global interrupt line at CPU level.*/
static inline void disable_global_irq(void) {
  CSR_CLR_BITS_IMM_MSTATUS(MSTATUS_MIE_BIT_MASK);
}

/*Enable the global interrupt line at CPU level.*/
static inline void enable_global_irq(void) {
  CSR_SET_BITS_IMM_MSTATUS(MSTATUS_MIE_BIT_MASK);
}

/*RISCV Interrupt IDs.*/
#define IRQ_ID_FAST_14 30
#define IRQ_ID_FAST_13 29
#define IRQ_ID_FAST_12 28
#define IRQ_ID_FAST_11 27
#define IRQ_ID_FAST_10 26
#define IRQ_ID_FAST_09 25
#define IRQ_ID_FAST_08 24
#define IRQ_ID_FAST_07 23
#define IRQ_ID_FAST_06 22
#define IRQ_ID_FAST_05 21
#define IRQ_ID_FAST_04 20
#define IRQ_ID_FAST_03 19
#define IRQ_ID_FAST_02 18
#define IRQ_ID_FAST_01 17
#define IRQ_ID_FAST_00 16
#define IRQ_ID_TIMER 7

/*Assignment of interrupts from BoxLambda components to RISCV fast IRQs*/
#define IRQ_ID_VERA IRQ_ID_FAST_13
#define IRQ_ID_VS_0 IRQ_ID_FAST_12
#define IRQ_ID_DMAC IRQ_ID_FAST_11
#define IRQ_ID_SDSPI IRQ_ID_FAST_10
#define IRQ_ID_GPIO IRQ_ID_FAST_09
#define IRQ_ID_USB_HID_1 IRQ_ID_FAST_08
#define IRQ_ID_USB_HID_0 IRQ_ID_FAST_07
#define IRQ_ID_I2C IRQ_ID_FAST_06
#define IRQ_ID_UART IRQ_ID_FAST_05
//IRQ_ID_FAST_04, 03 and 02 are currently not assigned.
#define IRQ_ID_DFX IRQ_ID_FAST_01
#define IRQ_ID_ICAP IRQ_ID_FAST_00

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

#ifdef __cplusplus
}
#endif
#endif /*INTERRUPTS_H*/
