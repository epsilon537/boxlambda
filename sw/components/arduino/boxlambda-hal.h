#ifndef BOXLAMBDA_HAL_H
#define BOXLAMBDA_HAL_H

#include "stdint.h"

#ifdef __cplusplus
extern "C" {
#endif

inline void analogReadResolution(int bits) {}
inline void analogWriteResolution(int bits) {}

#define LOW               0x0
#define HIGH              0x1

#define INPUT 0x0
#define OUTPUT 0x1
#define INPUT_PULLUP 0x2
#define INPUT_PULLDOWN 0x3

void pinMode(uint8_t pin, uint8_t mode);
void digitalWrite(uint16_t pin, uint8_t value);
int digitalRead(uint8_t pin);

inline int analogRead(uint8_t pin) { return 0; }
inline void analogWrite(uint8_t pin, int value) {}

#ifdef __cplusplus
}
#endif
#endif //BOXLAMBDA_HAL_H
