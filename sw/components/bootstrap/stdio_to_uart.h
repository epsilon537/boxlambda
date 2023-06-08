#ifndef STDIO_TO_UART_H
#define STDIO_TO_UART_H

#ifdef __cplusplus
extern "C" {
#endif

struct uart; /*forward declaration*/

//Map picolibc's stdio to the given UART instance.
void set_stdio_to_uart(struct uart *uartp);

#ifdef __cplusplus
}
#endif

#endif /*STDIO_TO_UART_H*/
