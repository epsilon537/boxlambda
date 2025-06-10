#include "Serial.h"

BoxLambdaSerial Serial;

//Speed in bps (baud)
void BoxLambdaSerial::begin(unsigned long speed) {
  uart_set_baudrate(speed);
}

