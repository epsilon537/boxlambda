#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "forth.h"

#ifdef __cplusplus
extern "C" {
#endif
//
// The following functions are implemented in the forth core (mecrisp-quintus-boxlambda.S file):
//
Forth_Datastack forth_core_init_(Forth_Datastack);
Forth_Datastack forth_repl_(Forth_Datastack);
Forth_Datastack forth_find_(Forth_Datastack);
Forth_Datastack forth_execute_(Forth_Datastack);
#ifdef __cplusplus
}
#endif

Forth_Datastack datastack = { 42, __datastack };

void forth_core_init() {
  datastack = forth_core_init_(datastack);
}

void forth_repl() {
  datastack = forth_repl_(datastack);
}

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

  datastack = forth_find_(datastack);
  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));

  forth_popda(); //drop flags
  return forth_popda();
}

void forth_execute_xt(uint32_t xt) {
  forth_pushda(xt);
  datastack = forth_execute_(datastack);
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

void forth_evaluate(const char *s, uint32_t count) {
  assert(s);

  //Push the given string on the stack, as input for setsource.
  forth_pushda((uint32_t)s);
  forth_pushda(count);

  //Evaluate the string.
  forth_execute_word("evaluate");
}

void forth_load_buf(char *s, bool verbose) {

  char *line_start_ptr = s;
  char *line_end_ptr;

  while (line_start_ptr) {
    line_end_ptr = strchr(line_start_ptr, '\n');

    if (line_end_ptr) {
      *line_end_ptr = 0; //0-terminate the line.

      if (verbose) printf("%s\n", line_start_ptr);

      forth_evaluate(line_start_ptr, strlen(line_start_ptr));
      line_start_ptr = line_end_ptr + 1;
    }
    else {
      line_start_ptr = 0;
    }
  }
}

