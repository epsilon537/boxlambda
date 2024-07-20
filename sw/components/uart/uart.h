#ifndef UART_H
#define UART_H

#include <stdint.h>
#include <stdarg.h>

#define PLATFORM_UART_BASE		0x10010000

#define PAD_RIGHT 1
#define PAD_ZERO  2

/* the following should be enough for 32 bit int */
#define PRINT_BUF_LEN 32

/* define LONG_MAX for int32 */
#define LONG_MAX 2147483647L

/* DETECTNULL returns nonzero if (long)X contains a NULL byte. */
#if LONG_MAX == 2147483647L
#define DETECTNULL(X) (((X) - 0x01010101) & ~(X) & 0x80808080)
#else
#if LONG_MAX == 9223372036854775807L
#define DETECTNULL(X) (((X) - 0x0101010101010101) & ~(X) & 0x8080808080808080)
#else
#error long int is not a 32bit or 64bit type.
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct uart
{
	volatile uint32_t * registers;
};

void uart_init(struct uart * module, volatile void * base_address);

void uart_configure(struct uart * module, uint32_t config);

void uart_set_baudrate(struct uart * module, uint32_t baudrate, uint32_t clk_freq);

int uart_tx_ready(struct uart * module);

void uart_tx(struct uart * module, uint8_t byte);

void uart_tx_string(struct uart * module, const char *str);

/*Returns non-zero if data available.*/
int uart_rx_ready(struct uart * module);

uint8_t uart_rx(struct uart * module);

uint32_t uart_rx_line(struct uart * module, char * str);

//int uart_printf(struct uart * module, const char * fmt, ...);

//int uart_scanf(struct uart * module, const char * fmt, ...);

int uart_puts(struct uart * module, const char *s);
int uart_printf(struct uart * module, const char *format, ...);
int uart_putchar(struct uart * module, int s);

#define UART_IRQ_TX_FIFO_HALF_EMPTY_MASK 0x8
#define UART_IRQ_TX_FIFO_EMPTY_MASK 0x4
#define UART_IRQ_RX_FIFO_HALF_FULL_MASK 0x2
#define UART_IRQ_RX_DATA_AVL_MASK 0x1

/*Retrieve uart ISR register to see which interrupts fired.*/
unsigned uart_get_isr(struct uart * module);

/*Retrieve uart IEN register to see which interrupts are enabled.*/
unsigned uart_get_ien(struct uart * module);

/*Acknowledge received interrupts. Bits set in the given mask will be cleared in the uart isr register.*/
void uart_irq_ack(struct uart * module, unsigned irq_mask);
/* Enable uart interrupts. Bits set enable corresponding uart interrupts. Successive enables are cumulative.
 * To disable uart irqs, used uart_irq_dis.*/
void uart_irq_en(struct uart * module, unsigned irq_mask);

/* Disable uart interrupts. Bits set disable corresponding uart interrupts. Successive disables are cumulative.
 * To enable uart irqs, used uart_irq_en.*/
void uart_irq_dis(struct uart * module, unsigned irq_mask);

#ifdef __cplusplus
}
#endif

#endif /* UART_H */

