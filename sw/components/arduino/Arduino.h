#ifndef ARDUINO_H
#define ARDUINO_H

// This is a minimal Arduino compatibility API with just enough functionality in place,
// or stubbed, to allow ulisp to build and boot. As the port of ulisp to BoxLambda
// fills out, it's expected that this API will fill out more as well.

#include "Serial.h"
#include "delay.h"
#include "SPI.h"
#include "Wire.h"
#include "GPIO.h"
#include "ADC.h"

#include <stdint.h>
#include <stdlib.h>

// A few definitions that didn't find a home in any of the submodules above.

#define PSTR(s) (s)

#define LED_BUILTIN 0

#define bitRead(value, bit) (((value) >> (bit)) & 0x01)

inline long random(long max) { return (long)(rand()) % max; }

inline void randomSeed(unsigned long seed) { srand(seed); }

#endif

