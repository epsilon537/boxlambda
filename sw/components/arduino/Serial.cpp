#include "Serial.h"

BoxLambdaSerial Serial;

//Speed in bps (baud)
void BoxLambdaSerial::begin(unsigned long speed) {
  //uart_configure(UART_SETUP_PFT_P_NONE, UART_SETUP_S_STOP_1, UART_SETUP_N_BPW_8, UART_SETUP_H_HFL_EN);
  uart_set_baudrate(speed);
}

