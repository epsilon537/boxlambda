#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "gpio.h"
#include "forth.h"
#include "testsuite.fs"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.

#ifdef __cplusplus
extern "C" {
#endif
//_init is executed by picolibc startup code before main().
void _init(void) {
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

#ifdef __cplusplus
}
#endif

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  forth_init();

  printf("Forth init complete.\n");

  printf("Executing testsuite...\n");

  forth_load_buf((char*)testsuite, false /*verbose*/);

  printf("Done.\n");

  forth_repl();

  printf("\nForth REPL exited.\n");

  while (1);
}

