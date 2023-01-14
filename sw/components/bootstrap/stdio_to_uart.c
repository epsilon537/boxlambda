#include <stdio.h>
#include "stdio_to_uart.h"
#include "platform.h"
#include "uart.h"

static struct uart *uartp = 0;

static int uart_putc(char c, FILE *file) {
  int res;
  
  (void) file;		/* Not used in this function */

  if (!uartp) {
    res = EOF;
  }
  else {
    while (!uart_tx_ready(uartp));
    uart_tx(uartp, (uint8_t)c);
    res = (int)c;
  }
  
  return res;
}

static int uart_getc(FILE *file) {
  int c;
  (void) file;		/* Not used in this function */

  if (!uartp) {
    c = EOF;
  }
  else {
    while (!uart_rx_ready(uartp));
    c = (int)uart_rx(uartp);
  }
  
  return c;
}

static FILE __stdio = FDEV_SETUP_STREAM(uart_putc,
					uart_getc,
					NULL,
					_FDEV_SETUP_RW);


FILE *const stdin = &__stdio;
FILE *const stdout = &__stdio;
FILE *const stderr = &__stdio;

void set_stdio_to_uart(struct uart *uart) {
  uartp = uart;
}
