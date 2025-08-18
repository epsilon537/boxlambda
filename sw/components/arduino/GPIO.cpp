#include "GPIO.h"

void pinMode(uint8_t pin, uint8_t mode) {
//  uint32_t dir = gpio_get_direction();
//  uint32_t msk = 1<<pin;
//
//  if (mode == OUTPUT)
//    dir |= msk:
//  else
//    dir &= ~msk;
//
//  gpio_set_direction(dir);
}

void digitalWrite(uint16_t pin, uint8_t value) {
//  gpio_set_pin_value(pin , (int)value);
}

int digitalRead(uint8_t pin) {
//  return gpio_get_input() & (1<<pin) ? 1 : 0;
  return 0;
}
