#include <stdint.h>
#include <string.h>
#include <assert.h>
#include "forth.h"

Forth_Datastack datastack = { 42, __datastack };

void forth_pushda(uint32_t val) {
  --(datastack.psp);

  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));

  *(datastack.psp) = datastack.tos;
  (datastack.tos) = val;
}

uint32_t forth_popda() {
  uint32_t res = datastack.tos;

  datastack.tos = *(datastack.psp);
  ++(datastack.psp);

  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));

  return res;
}

uint32_t forth_find_word(const char *s) {
  assert(s);

  forth_pushda((uint32_t)s);
  forth_pushda(strlen(s));

  datastack = forth_find(datastack);
  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));

  forth_popda(); //drop flags
  return forth_popda();
}

void forth_execute_xt(uint32_t xt) {
  forth_pushda(xt);
  datastack = forth_execute(datastack);
  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));
}

uint32_t forth_execute_word(const char *s) {
  uint32_t xt = forth_find_word(s);

  if (xt) {
    forth_execute_xt(xt);
    return 0;
  }
  else {
    return -1;
  }
}

void forth_eval(const char *s) {
  assert(s);

  //Retrieve current input source and put it on the stack.
  forth_execute_word("source");
  forth_pushda((uint32_t)s);
  forth_pushda(strlen(s));

  //Switch source to the given string.
  forth_execute_word("setsource");

  //Evaluate the string.
  forth_execute_word("interpret");
  //Restore original source
  forth_execute_word("setsource");
}

