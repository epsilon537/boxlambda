#include "stdio_stream.h"
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

FILE stdin_stream = FDEV_SETUP_STREAM(
 NULL,
 uart_getc,
 NULL,
 _FDEV_SETUP_READ);

FILE stdout_stream = FDEV_SETUP_STREAM(
 uart_putc,
 NULL,
 NULL,
 _FDEV_SETUP_WRITE);

FILE *const stdin = &stdin_stream;
FILE *const stdout = &stdout_stream;
FILE *const stderr = &stdout_stream;


