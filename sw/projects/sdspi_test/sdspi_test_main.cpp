#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdtest.h"
#include "interrupts.h"

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

//_init is executed by picolibc startup code before main().
void _init(void) {
  disable_all_irqs();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  enable_global_irq();

  if (sdspi_test() == 0)
    printf("Test Successful.\n");
  else
    printf("SDSPI Test failed.\n");

  while(1);

  return 0;
}
