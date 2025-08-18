#pragma once

#include "stdint.h"

#ifdef __cplusplus
extern "C" {
#endif

// From Arduino's adc API
inline void analogReadResolution(int bits) {}
inline void analogWriteResolution(int bits) {}
inline int analogRead(uint8_t pin) { return 0; }
inline void analogWrite(uint8_t pin, int value) {}


#ifdef __cplusplus
}
#endif
