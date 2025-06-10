#ifndef UART_H
#define UART_H

#include <stdint.h>
#include <stdarg.h>
#include "uart_regs.h"

#ifdef __cplusplus
extern "C" {
#endif

void uart_configure(uart_setup_pft_t parity, uart_setup_s_t stop_bits, uart_setup_n_t bits_per_word, uart_setup_h_t hw_flow_control);

void uart_set_baudrate(uint32_t baudrate);

int uart_tx_ready(void);

void uart_tx(uint8_t byte);

void uart_tx_string(const char *str);

//Block until the TX FIFO is empty.
void uart_tx_flush(void);

/*Returns non-zero if data available.*/
int uart_rx_ready(void);

uint8_t uart_rx(void);

uint32_t uart_rx_line(char * str);

int uart_puts(const char *s);
int uart_printf(const char *format, ...);
int uart_putchar(int s);

/*Retrieve uart ISR register to see which interrupts fired.*/
uint32_t uart_get_isr(void);

/*Retrieve uart IEN register to see which interrupts are enabled.*/
uint32_t uart_get_ien(void);

/*Acknowledge received interrupts. Bits set in the given mask will be cleared in the uart isr register.*/
void uart_irq_ack(uint32_t irq_mask);

/* Enable uart interrupts. Bits set enable corresponding uart interrupts. Successive enables are cumulative.
 * To disable uart irqs, used uart_irq_dis.*/
void uart_irq_en(uint32_t irq_mask);

/* Disable uart interrupts. Bits set disable corresponding uart interrupts. Successive disables are cumulative.
 * To enable uart irqs, used uart_irq_en.*/
void uart_irq_dis(uint32_t irq_mask);

#ifdef __cplusplus
}
#endif

#endif /* UART_H */

