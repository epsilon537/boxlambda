#include "uart.h"
#include <stdint.h>
#include <string.h>
#include "mcycle.h"

#define PAD_RIGHT 1
#define PAD_ZERO  2

/* This constant has to match the hardware */
#define TX_FIFO_SZ 128

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

static uint32_t remu10(uint32_t n);
static void qprintchar(char **str, int c);
static int qprints(char **out, const char *string, int width, int pad);
static int qprinti(char **out, int i, int b, int sg, int width, int pad, char letbase);
static int qprint(char **out, const char *format, va_list va);

void uart_configure(uart_setup_pft_t parity, uart_setup_s_t stop_bits, uart_setup_n_t bits_per_word, uart_setup_h_t hw_flow_control)
{
  uart_setup_t setup{.PFT=parity, .S=stop_bits, .N=bits_per_word, .H=hw_flow_control};
  UART->SETUP = setup.UINT32;
}

void uart_tx_flush(void)
{
  while (UART->FIFO_bf.TX_AVL != (TX_FIFO_SZ-1));
}

int uart_tx_ready(void)
{
  return UART->FIFO_bf.TX_Z;
}

void uart_tx(uint8_t byte)
{
  UART->TXDATA = (uint32_t)byte;
}

void uart_tx_string(const char *str)
{
  for (uint32_t i = 0; str[i] != '\0'; i++) {
    while (!uart_tx_ready());
    uart_tx((uint8_t)str[i]);
  }
}

int uart_rx_ready()
{
  return UART->FIFO_bf.RX_Z;
}

uint8_t uart_rx()
{
  return (uint8_t)(UART->RXDATA_bf.RWORD);
}

uint32_t uart_rx_line(char * str)
{
  uint32_t i = 0;
  for (;;) {
    while (!uart_rx_ready());
    str[i] = uart_rx();
    if (str[i] == '\n')
      break;
    i++;
  }
  str[i] = '\0';
  return i;
}

void uart_set_baudrate(uint32_t baudrate)
{
  UART->SETUP_bf.BAUD_CLKS = PLATFORM_CLK_FREQ / baudrate;
}

void uart_irq_ack(uint32_t irq_mask) {
  UART->ISR = irq_mask;
}

void uart_irq_en(uint32_t irq_mask) {
  UART->IEN |= irq_mask;
}

void uart_irq_dis(uint32_t irq_mask) {
  UART->IEN &= ~irq_mask;
}

uint32_t uart_get_isr(void) {
  return UART->ISR;
}

uint32_t uart_get_ien(void) {
  return UART->IEN;
}

/* Nonzero if either X or Y is not aligned on a "long" boundary. */
#define UNALIGNED(X, Y) \
  (((long)X & (sizeof (long) - 1)) | ((long)Y & (sizeof (long) - 1)))

static uint32_t divu10(uint32_t n) {
  uint32_t q, r;

  q = (n >> 1) + (n >> 2);
  q = q + (q >> 4);
  q = q + (q >> 8);
  q = q + (q >> 16);
  q = q >> 3;
  r = n - q * 10;

  return q + ((r + 6) >> 4);
}

char remu10_table[16] = {
  0, 1, 2, 2, 3, 3, 4, 5,
  5, 6, 7, 7, 8, 8, 9, 0
};

static uint32_t remu10(uint32_t n) {
  n = (0x19999999 * n + (n >> 1) + (n >> 3)) >> 28;
  return remu10_table[n];
}

int uart_putchar(int s)
{
  while (!uart_tx_ready());
  uart_tx((uint8_t)s);
  return s;
}

static void qprintchar(char **str, int c)
{
  if (str)
  {
    **str = c;
    ++(*str);
  }
  else
    uart_putchar((char)c);
}

static int qprints(char **out, const char *string, int width, int pad)
{
  int pc = 0, padchar = ' ';

  if (width > 0) {
    int len = 0;
    const char *ptr;
    for (ptr = string; *ptr; ++ptr) ++len;
    if (len >= width) width = 0;
    else width -= len;
    if (pad & PAD_ZERO) padchar = '0';
  }
  if (!(pad & PAD_RIGHT)) {
    for ( ; width > 0; --width) {
      qprintchar (out, padchar);
      ++pc;
    }
  }
  for ( ; *string ; ++string) {
    qprintchar (out, *string);
    ++pc;
  }
  for ( ; width > 0; --width) {
    qprintchar (out, padchar);
    ++pc;
  }

  return pc;
}

static int qprinti(char **out, int i, int b, int sg, int width, int pad, char letbase)
{
  char print_buf[PRINT_BUF_LEN];
  char *s;
  int neg = 0, pc = 0;
  uint32_t u = i;

  if (i == 0)
  {
    print_buf[0] = '0';
    print_buf[1] = '\0';
    return qprints (out, print_buf, width, pad);
  }

  if (sg && b == 10 && i < 0)
  {
    neg = 1;
    u = -i;
  }

  s = print_buf + PRINT_BUF_LEN-1;
  *s = '\0';

  // treat HEX and decimal differently
  if(b == 16) {
    // HEX
    while (u) {
      int t = u & 0xF;

      if (t >= 10)
        t += letbase - '0' - 10;

      *--s = t + '0';
      u >>= 4;
    }
  } else {
    // decimal
    while (u) {
      *--s = remu10(u) + '0';
      u = divu10(u);
    }
  }

  if (neg) {
    if( width && (pad & PAD_ZERO) )
    {
      qprintchar (out, '-');
      ++pc;
      --width;
    }
    else
    {
      *--s = '-';
    }
  }
  return pc + qprints (out, s, width, pad);
}

static int qprint(char **out, const char *format, va_list va)
{
  int width, pad;
  int pc = 0;
  char scr[2];

  for (; *format != 0; ++format)
  {
    if (*format == '%')
    {
      ++format;
      width = pad = 0;
      if (*format == '\0') break;
      if (*format == '%') goto out;
      if (*format == '-')
      {
        ++format;
        pad = PAD_RIGHT;
      }
      while (*format == '0')
      {
        ++format;
        pad |= PAD_ZERO;
      }
      for ( ; *format >= '0' && *format <= '9'; ++format) {
        width *= 10;
        width += *format - '0';
      }
      if( *format == 's' ) {
        char *s = va_arg(va, char*);
        pc += qprints (out, s?s:"(null)", width, pad);
        continue;
      }
      if( *format == 'd' ) {
        pc += qprinti (out, va_arg(va, int), 10, 1, width, pad, 'a');
        continue;
      }
      if( *format == 'u' ) {
        pc += qprinti (out, va_arg(va, uint32_t), 10, 0, width, pad, 'a');
        continue;
      }
      if( *format == 'x' ) {
        pc += qprinti (out, va_arg(va, uint32_t), 16, 0, width, pad, 'a');
        continue;
      }
      if( *format == 'X' ) {
        pc += qprinti (out, va_arg(va, uint32_t), 16, 0, width, pad, 'A');
        continue;
      }
      if( *format == 'c' ) {
        scr[0] = va_arg(va, int);
        scr[1] = '\0';
        pc += qprints (out, scr, width, pad);
        continue;
      }
    }
    else
    {
out:
      qprintchar (out, *format);
      ++pc;
    }
  }
  if (out) **out = '\0';

  return pc;
}


int uart_puts(const char *s)
{
  int i=0;

  while(s[i] != '\0')
    uart_putchar(s[i++]);

  uart_putchar('\n');

  return i;
}

int uart_printf(const char *format, ...)
{
  int pc;
  va_list va;

  va_start(va, format);

  pc = qprint(0, format, va);

  va_end(va);

  return pc;

}


