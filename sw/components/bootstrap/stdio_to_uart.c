#include <stdio.h>
#include "uart.h"

static int uart_putc(char c, FILE *file) {
  int res;

  (void) file;		/* Not used in this function */

  {
    while (!uart_tx_ready());
    uart_tx((uint8_t)c);
    res = (int)c;
  }

  return res;
}

static int uart_getc(FILE *file) {
  int c;
  (void) file;		/* Not used in this function */

  {
    while (!uart_rx_ready());
    c = (int)uart_rx();
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

