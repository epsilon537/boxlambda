#ifndef ARDUINO_H
#define ARDUINO_H

#include "Serial.h"
#include "delay.h"
#include "SPI.h"
#include "Wire.h"
#include "boxlambda-hal.h"

#include <stdint.h>
#include <stdlib.h>

#define PSTR(s) (s)

#define LED_BUILTIN 0

#define bitRead(value, bit) (((value) >> (bit)) & 0x01)

inline long random(long max) { return (long)(rand()) % max; }

inline void randomSeed(unsigned long seed) { srand(seed); }

#endif

