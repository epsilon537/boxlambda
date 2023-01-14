#ifndef STDIO_TO_UART_H
#define STDIO_TO_UART_H

struct uart; /*forward declaration*/

//Map picolibc's stdio to the given UART instance.
void set_stdio_to_uart(struct uart *uartp);

#endif /*STDIO_TO_UART_H*/
