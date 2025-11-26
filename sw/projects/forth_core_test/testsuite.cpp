#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "gpio.h"
#include "forth.h"
#include "testsuite.fs"
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

//This function will be registered with Forth as a test.
void test_c_fun() {
  uint32_t sp;
  __asm__ volatile("mv %0, sp;" : "=r"(sp));

  //RISC-V ABI expected stack pointer to be 16-byte aligned
  assert((sp & 15) == 0);

  uint32_t first_arg = forth_popda();
  uint32_t second_arg = forth_popda();

  printf("test_c_fun called with args %d and %d.\n", first_arg, second_arg);
  printf("Returning values 77 88.\n");

  forth_pushda(77);
  forth_pushda(88);
}

#ifdef __cplusplus
}
#endif

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  forth_core_init();

  printf("Forth init complete.\n");

  printf("Compiling Forth included_tools...\n");

  forth_load_buf((char*)included_tools, /*verbose=*/ false);

  printf("Forth-C FFI testcases...\n");

  forth_pushda(forth_find_word("quit"));
  printf("Checking forth_find_word(quit) against ' quit. Diff should be 0:\n");
  forth_eval("' quit  - . cr");

  printf("Forth computing 3 4 +:\n");
  forth_pushda(3);
  forth_pushda(4);
  forth_execute_word("+");
  printf("Result: %d\n", forth_popda());

  printf("Forth evaluating string: 3 4 + . cr\n");
  forth_eval("3 4 + . cr");
  printf("Forth evaluating string: 42 emit cr\n");
  forth_eval("42 emit cr");

  printf("Registering test_c_fun with Forth...\n");
  forth_register_cfun(test_c_fun, "test_c_fun");
  printf("Forth evaluating string: 11 22 test_c_fun . . cr with various RS alignments...\n");
  forth_eval("11 22 test_c_fun . . cr");
  forth_eval("11 22 0 >r test_c_fun rdrop . . cr");
  forth_eval("11 22 0 >r 0 >r test_c_fun rdrop rdrop . . cr");
  forth_eval("11 22 0 >r 0 >r 0 >r test_c_fun rdrop rdrop rdrop . . cr");
  forth_eval("11 22 0 >r 0 >r 0 >r 0 >r test_c_fun rdrop rdrop rdrop rdrop . . cr");

  printf("Executing .s:\n");
  forth_execute_word(".s");

  printf("Executing Forth testsuite...\n");

  forth_load_buf((char*)testsuite, false /*verbose*/);

  forth_repl();

  printf("Done.\n");

  while (1);
}

