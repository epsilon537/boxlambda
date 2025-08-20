#ifndef SERIAL_ADAPTER_H
#define SERIAL_ADAPTER_H

#include <stddef.h>
#include <stdint.h>
#include "uart.h"

// BoxLambda adapter of Arduino's Serial

class BoxLambdaSerial {
public:
  //Speed in bps (baud)
  void begin(unsigned long speed);

  // void begin(unsigned long baudrate, uint16_t config) = 0;

  // void end();

  //Returns number of characters available for reading.
  inline int available(void) {
    return uart_rx_ready();
  }

  // Returns first byte of incoming data or -1 if no data is available.
  inline int read(void) {
    if (!uart_rx_ready()) return -1;

    return (int)uart_rx();
  }

  //Waits for the transmission of outgoing serial data to complete.
  inline void flush(void) {
    uart_tx_flush();
  }

  //Write a single character. Returns number of bytes written.
  inline size_t write(uint8_t val) {
    //Wait until there's space...
    while (!uart_tx_ready());
    uart_tx((uint8_t)val);

    return 1;
  }

  //Returns true if the serial port is available.
  inline operator bool() { return true; }
};


extern BoxLambdaSerial Serial;

#endif //SERIAL_ADAPTER
