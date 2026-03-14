#include <assert.h>
#include <stdio.h>
#include "forth.h"

//This function will be registered with Forth as a test.
void test_c_fun() {
  uint32_t sp;
  __asm__ volatile("mv %0, sp;" : "=r"(sp));

  //RISC-V ABI expected stack pointer to be 16-byte aligned
  assert((sp & 15) == 0);

  uint32_t first_arg = forth_popda();
  uint32_t second_arg = forth_popda();

  printf("test_c_fun called with args %d and %d.\n", first_arg, second_arg);

  //Put them back on the stack, reversed
  forth_pushda(first_arg);
  forth_pushda(second_arg);
  //Create a test Word in Forth to call so we have c calls forth calls c calls forth.
  forth_eval(": foo .\" In foo...\" 2dup . . cr + ;");

  printf("Calling test Word foo in Forth with args %d %d...\n", second_arg, first_arg);

  forth_execute_word("foo");

  uint32_t res = forth_popda();

  printf("Foo returned %d.\n", res);

  printf("Returning values 77 88 to Forth.\n");

  forth_pushda(77);
  forth_pushda(88);
}

void forth_core_test() {
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
  forth_eval(": test_c_fun_ret_s s\" test_c_fun returned \" ;");
  forth_eval("11 22 test_c_fun .( test_c_fun returned ) . . cr");
  forth_eval("11 22 0 >r test_c_fun rdrop .( test_c_fun returned ) . . cr");
  forth_eval("11 22 0 >r 0 >r test_c_fun rdrop rdrop .( test_c_fun returned ) . . cr");
  forth_eval("11 22 0 >r 0 >r 0 >r test_c_fun rdrop rdrop rdrop .( test_c_fun returned ) . . cr");
  forth_eval("11 22 0 >r 0 >r 0 >r 0 >r test_c_fun rdrop rdrop rdrop rdrop .( test_c_fun returned ) . . cr");

  printf("Executing .s:\n");
  forth_execute_word(".s");
}

