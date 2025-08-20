#pragma once

#include "stdint.h"

#ifdef __cplusplus
extern "C" {
#endif

// From Arduino's gpio API
#define LOW               0x0
#define HIGH              0x1

#define INPUT 0x0
#define OUTPUT 0x1
#define INPUT_PULLUP 0x2
#define INPUT_PULLDOWN 0x3

void pinMode(uint8_t pin, uint8_t mode);
void digitalWrite(uint16_t pin, uint8_t value);
int digitalRead(uint8_t pin);

#ifdef __cplusplus
}
#endif
