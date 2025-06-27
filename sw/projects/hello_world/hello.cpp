#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.

typedef union {
  uint32_t SETUP;
  struct {
    uint32_t BAUD_CLKS : 24; // System clock per baudrate interval
    uint32_t PFT : 3; // Parity setup
    uint32_t S : 1; // Number of stop bits - 1
    uint32_t N : 2; // 8 - number of bits per word
    uint32_t H : 1; // Disable hardware flow control
  };
} setup_t;

//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  setup_t setup;

  setup.SETUP = 0x12345678;
  printf("Baud = %d\n", setup.BAUD_CLKS);

  setup.PFT = 0;
  printf("setup =0x%x\n", setup.SETUP);

  uint32_t leds = 0xF;

  gpio_init();

  printf("Hello, World!\n");

  //GPIO bits 7:4 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input() & 0xf0) == GPIO_SIM_INDICATOR)
    printf("This is a simulation.\n");
  else
    printf("This is not a simulation.\n");

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

