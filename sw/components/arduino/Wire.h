#ifndef TwoWire_h
#define TwoWire_h

#include <stddef.h>
#include <stdint.h>

// A BoxLambda adaption of Arduino's I2C Wire API.
class BoxLambdaWire
{
  public:
    inline BoxLambdaWire() {};
    inline void begin() {}
    inline void beginTransmission(uint8_t address) {}
    inline uint8_t endTransmission(bool sendStop) { return 0; }
    inline uint8_t endTransmission(void) { return 0; }
    inline uint8_t requestFrom(uint8_t address, uint8_t sendStop) { return 0; }
    inline size_t write(uint8_t) { return 0; }
    inline int read(void) { return 0; }
};

extern BoxLambdaWire Wire;

#endif
