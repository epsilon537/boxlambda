#include "Serial.h"
#include "stdio_to_uart.h"
#include "mcycle.h"

BoxLambdaSerial Serial;

BoxLambdaSerial::BoxLambdaSerial() {
  uart_init(&uart0_, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0_, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0_);
}

//Speed in bps (baud)
void BoxLambdaSerial::begin(unsigned long speed) {
  uart_set_baudrate(&uart0_, speed, PLATFORM_CLK_FREQ);
}

