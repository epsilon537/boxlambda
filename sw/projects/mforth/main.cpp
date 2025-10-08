#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "gpio.h"
#include "forth.h"
#include "included-tools.fs"

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

void void0() {
 printf("\nvoid0 in C.\n");
}

void void1(uint32_t a0) {
 printf("\nvoid1 in C. arg: %d\n", a0);
}

void void2(uint32_t a0, uint32_t a1) {
 printf("\nvoid2 in C. arg0: %d, arg1: %d\n", a0, a1);
}

uint32_t onerettwoargs(uint32_t a0, uint32_t a1) {
 uint32_t sum = a0+a1;
 printf("\nvoid2 in C. arg0: %d, arg1: %d, return: %d\n", a0, a1, sum);
 return sum;
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

  printf("Compiling Forth included_tools...\n");

  forth_load_buf((char*)included_tools);

  printf("Done.\n");

  printf("Forth quit is at %d.\n", forth_find_word("quit"));

  printf("Executing .s:\n");

  forth_execute_word(".s");

  printf("\n");

  printf("Forth computing 3 4 +:\n");
  forth_pushda(3);
  forth_pushda(4);
  forth_execute_word("+");
  printf("Result: %d\n", forth_popda());

  printf("Forth evaluating string: 3 4 + . cr\n");
  forth_eval("3 4 + . cr");
  printf("Forth evaluating string: 42 emit cr\n");
  forth_eval("42 emit cr");

  printf("Registering C functions.\n");

  forth_register_fun(void0, 0, 0);
  forth_register_fun(void1, 1, 0);
  forth_register_fun(void2, 2, 0);
  forth_register_fun(onerettwoargs, 2, 1);

  forth_execute_word("welcome");
  forth_repl();

  printf("\nForth REPL exited.\n");

  while (1);
}

