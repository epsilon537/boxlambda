#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "gpio.h"
#include "forth.h"

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

const char evalStr[] = "3 4 + . cr";

#ifdef __cplusplus
}
#endif

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  forth_init();

  printf("Forth init complete.\n");

  printf("Forth quit is at %d.\n", forth_find_word("quit"));

  printf("Executing .s:\n");

  forth_execute_word(".s");

  printf("\n");

  printf("Forth computing 3 4 +:\n");
  forth_pushda(3);
  forth_pushda(4);
  forth_execute_word("+");
  printf("Result: %d\n", forth_popda());

  printf("Forth evaluating string: %s\n", evalStr);
  forth_eval(evalStr);

  printf("REPL:\n");
  printf("-----\n");

  forth_repl();

  printf("\nForth REPL exited.\n");

  while (1);
}

