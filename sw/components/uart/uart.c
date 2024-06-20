
#include "uart.h"
#include <stdint.h>
//#include <stdio.h>
#include <string.h>


#define UART_REG_SETUP    0
#define UART_REG_FIFO    1
#define UART_REG_RXDATA    2
#define UART_REG_TXDATA    3
#define UART_REG_ISR       4
#define UART_REG_IEN       5

static unsigned remu10(unsigned n);
static void qprintchar(struct uart * module, char **str, int c);
static int qprints(struct uart * module, char **out, const char *string, int width, int pad);
static int qprinti(struct uart * module, char **out, int i, int b, int sg, int width, int pad, char letbase);
static int qprint(struct uart * module, char **out, const char *format, va_list va);

void uart_init(struct uart * module, volatile void * base_address)
{
  module->registers = (volatile uint32_t *)base_address;
}

void uart_configure(struct uart * module, uint32_t config)
{
  module->registers[UART_REG_SETUP] = config;
}

int uart_tx_ready(struct uart * module)
{
  return module->registers[UART_REG_FIFO] & 0x00010000;
}

void uart_tx(struct uart * module, uint8_t byte)
{
  module->registers[UART_REG_TXDATA] = (uint32_t)byte;
}

void uart_tx_string(struct uart * module, const char *str)
{
  for (uint32_t i = 0; str[i] != '\0'; i++) {
    while (!uart_tx_ready(module))
      ;
    uart_tx(module, (uint8_t)str[i]);
  }
}

int uart_rx_ready(struct uart * module)
{
  return module->registers[UART_REG_FIFO] & 0x00000001;
}

uint8_t uart_rx(struct uart * module)
{
  return (uint8_t)(module->registers[UART_REG_RXDATA] & 0x000000FF);
}

uint32_t uart_rx_line(struct uart * module, char * str)
{
  uint32_t i = 0;
  for (;;) {
    while (!uart_rx_ready(module))
      ;
    str[i] = uart_rx(module);
    if (str[i] == '\n')
      break;
    i++;
  }
  str[i] = '\0';
  return i;
}

void uart_set_baudrate(struct uart * module, uint32_t baudrate, uint32_t clk_freq)
{
  module->registers[UART_REG_SETUP] = clk_freq / baudrate;
}

void uart_irq_ack(struct uart * module, unsigned irq_mask) {
  module->registers[UART_REG_ISR] = irq_mask;
}

void uart_irq_en(struct uart * module, unsigned irq_mask) {
  module->registers[UART_REG_IEN] = irq_mask | module->registers[UART_REG_IEN];
}

void uart_irq_dis(struct uart * module, unsigned irq_mask) {
  module->registers[UART_REG_IEN] = ~irq_mask & module->registers[UART_REG_IEN];
}

unsigned uart_get_isr(struct uart * module) {
  return module->registers[UART_REG_ISR];
}

unsigned uart_get_ien(struct uart * module) {
  return module->registers[UART_REG_IEN];
}

//int uart_printf(struct uart * module, const char * fmt, ...)
//{
//  int result;
//  char str[100];
//
//  va_list argp;
//  va_start(argp, fmt);
//  result = vsnprintf(str, 100, fmt, argp);
//  va_end(argp);
//  uart_tx_string(module, str);
//
//  return result;
//}
//
//int uart_scanf(struct uart * module, const char * fmt, ...)
//{
//  char str[100];
//  int result;
//  uart_rx_line(module, str);
//
//  va_list argp;
//  va_start(argp, fmt);
//  result = vsscanf(str, fmt, argp);
//  va_end(argp);
//  return result;
//}

/* Nonzero if either X or Y is not aligned on a "long" boundary. */
#define UNALIGNED(X, Y) \
  (((long)X & (sizeof (long) - 1)) | ((long)Y & (sizeof (long) - 1)))

static unsigned divu10(unsigned n) {
  unsigned q, r;

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

static unsigned remu10(unsigned n) {
  n = (0x19999999 * n + (n >> 1) + (n >> 3)) >> 28;
  return remu10_table[n];
}


int uart_putchar(struct uart * module, int s)
{
  while (!uart_tx_ready(module))
      ;
  uart_tx(module, (uint8_t)s);
  return s;
}

static void qprintchar(struct uart * module, char **str, int c)
{
  if (str)
  {
    **str = c;
    ++(*str);
  }
  else
    uart_putchar(module, (char)c);
}

static int qprints(struct uart * module, char **out, const char *string, int width, int pad)
{
  register int pc = 0, padchar = ' ';

  if (width > 0) {
    register int len = 0;
    register const char *ptr;
    for (ptr = string; *ptr; ++ptr) ++len;
    if (len >= width) width = 0;
    else width -= len;
    if (pad & PAD_ZERO) padchar = '0';
  }
  if (!(pad & PAD_RIGHT)) {
    for ( ; width > 0; --width) {
      qprintchar (module, out, padchar);
      ++pc;
    }
  }
  for ( ; *string ; ++string) {
    qprintchar (module, out, *string);
    ++pc;
  }
  for ( ; width > 0; --width) {
    qprintchar (module, out, padchar);
    ++pc;
  }

  return pc;
}

static int qprinti(struct uart * module, char **out, int i, int b, int sg, int width, int pad, char letbase)
{
  char print_buf[PRINT_BUF_LEN];
  register char *s;
  register int neg = 0, pc = 0;
  unsigned int u = i;

  if (i == 0)
  {
    print_buf[0] = '0';
    print_buf[1] = '\0';
    return qprints (module, out, print_buf, width, pad);
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
      qprintchar (module, out, '-');
      ++pc;
      --width;
    }
    else
    {
      *--s = '-';
    }
  }
  return pc + qprints (module, out, s, width, pad);
}

static int qprint(struct uart * module, char **out, const char *format, va_list va)
{
  register int width, pad;
  register int pc = 0;
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
        register char *s = va_arg(va, char*);
        pc += qprints (module, out, s?s:"(null)", width, pad);
        continue;
      }
      if( *format == 'd' ) {
        pc += qprinti (module, out, va_arg(va, int), 10, 1, width, pad, 'a');
        continue;
      }
      if( *format == 'u' ) {
        pc += qprinti (module, out, va_arg(va, unsigned int), 10, 0, width, pad, 'a');
        continue;
      }
      if( *format == 'x' ) {
        pc += qprinti (module, out, va_arg(va, uint32_t), 16, 0, width, pad, 'a');
        continue;
      }
      if( *format == 'X' ) {
        pc += qprinti (module, out, va_arg(va, uint32_t), 16, 0, width, pad, 'A');
        continue;
      }
      if( *format == 'c' ) {
        scr[0] = va_arg(va, int);
        scr[1] = '\0';
        pc += qprints (module, out, scr, width, pad);
        continue;
      }
    }
    else
    {
out:
      qprintchar (module, out, *format);
      ++pc;
    }
  }
  if (out) **out = '\0';

  return pc;
}


int uart_puts(struct uart * module, const char *s)
{
  int i=0;

  while(s[i] != '\0')
    uart_putchar(module, s[i++]);

  uart_putchar(module, '\n');

  return i;
}

int uart_printf(struct uart * module, const char *format, ...)
{
  int pc;
  va_list va;

  va_start(va, format);

  pc = qprint(module, 0, format, va);

  va_end(va);

  return pc;

}


