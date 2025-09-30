#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "gpio.h"

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

void mForthInit();
void mForthREPL();

#ifdef __cplusplus
}
#endif

extern char __forth_ram_start[];
extern char __forth_ram_end[];
extern char __forth_imem_start[];
extern char __forth_imem_end[];

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  memset(__forth_ram_start, 0xff, __forth_ram_end - __forth_ram_start);
  memset(__forth_imem_start, 0xff, __forth_imem_end - __forth_imem_start);

  mForthInit();

  printf("mForth init complete. Starting REPL.\n");
  printf("------------------------------------\n");

  mForthREPL();

  printf("mForth REPL exited.\n");

  while (1);
}

