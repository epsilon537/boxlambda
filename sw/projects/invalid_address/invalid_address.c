#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.
//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

/* This test checks that accessing invalid addresses don't cause */
/* cpu exceptions and reading from invalid addresses returns 0xdeadbeef. */
int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  //GPIO bits 7:4 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input() & 0xf0) == GPIO_SIM_INDICATOR)
    printf("This is a simulation.\n");
  else
    printf("This is not a simulation.\n");

  printf("Writing 0 to invalid address 0x18000000:\n");
  *(volatile unsigned *)0x18000000 = 0;
  printf("Reading from invalid address 0x18000000:\n");
  printf("0x18000000: 0x%x\n", *(volatile unsigned *)0x18000000);
  assert(*(volatile unsigned *)0x18000000 == 0xdeadbeef);

  printf("Writing 0 to invalid address 0x10080000:\n");
  *(volatile unsigned *)0x10080000 = 0;
  printf("Reading from invalid address 0x10080000:\n");
  printf("0x10080000: 0x%x\n", *(volatile unsigned *)0x10080000);
  assert(*(volatile unsigned *)0x10080000 == 0xdeadbeef);

  printf("Writing 0 to invalid address 0x20000000:\n");
  *(volatile unsigned *)0x20000000 = 0;
  printf("Reading from invalid address 0x20000000:\n");
  printf("0x20000000: 0x%x\n", *(volatile unsigned *)0x20000000);
  assert(*(volatile unsigned *)0x20000000 == 0xdeadbeef);

  printf("Writing 0 to invalid address 0x10000400:\n");
  *(volatile unsigned *)0x10000400 = 0;
  printf("Reading from invalid address 0x10000400:\n");
  printf("0x10000400: 0x%x\n", *(volatile unsigned *)0x10000400);
  assert(*(volatile unsigned *)0x10000400 == 0xdeadbeef);

  printf("Test Successful.\n");

  for (;;) {
    gpio_set_output(leds);
    leds ^= 0xF;

    if ((gpio_get_input() & 0xf0) == GPIO_SIM_INDICATOR)
      usleep(500 * 10); //Sleep less when we're running inside a simulator.
    else
      usleep(500 * 1000);
  }
}

