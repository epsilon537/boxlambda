#ifndef SERIAL_ADAPTER_H
#define SERIAL_ADAPTER_H

#include <stddef.h>
#include <stdint.h>
#include "uart.h"

// BoxLambda adapter of Arduino's Seral

class BoxLambdaSerial {
public:
  BoxLambdaSerial();

  //Speed in bps (baud)
  void begin(unsigned long speed);

  // void begin(unsigned long baudrate, uint16_t config) = 0;

  // void end();

  //Returns number of characters available for reading.
  inline int available(void) {
    return uart_rx_ready(&uart0_);
  }

  // Returns first byte of incoming data or -1 if no data is available.
  inline int read(void) {
    if (!uart_rx_ready(&uart0_)) return -1;

    return (int)uart_rx(&uart0_);
  }

  //Waits for the transmission of outgoing serial data to complete.
  inline void flush(void) {
    uart_tx_flush(&uart0_);
  }

  //Write a single character. Returns number of bytes written.
  inline size_t write(uint8_t val) {
    //Wait until there's space...
    while (!uart_tx_ready(&uart0_));
    uart_tx(&uart0_, (uint8_t)val);

    return 1;
  }

  //Returns true if the serial port is available.
  inline operator bool() { return true; }

private:
   struct uart uart0_;
};


extern BoxLambdaSerial Serial;

#endif //SERIAL_ADAPTER
