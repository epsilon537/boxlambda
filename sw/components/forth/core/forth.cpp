#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "forth.h"
#include "ff.h"
#include "fatal.h"

#ifdef __cplusplus
extern "C" {
#endif
//
// The following functions are implemented in the forth core.
// They operate on the datastack object below.
//
void forth_core_init_(void); //Initialize the Forth Core environment
void forth_core_fun_(void); //Invoke the Forth Core Word (e.g. execute) on the top of the stack.

void find(void);
void execute(void);

Forth_Datastack datastack; //Initialized by forth_core_init.

uint32_t evaluate_xt = 0;

#ifdef __cplusplus
}
#endif

void forth_core_init() {
  //forth_core_init_ will set up the datastack object.
  forth_core_init_();

  evaluate_xt = forth_find_word("evaluate");
  assert(evaluate_xt);
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
  forth_pushda((uint32_t)find);
  forth_core_fun_();
  assert((datastack.psp >= __datastack_end) && (datastack.psp <= __datastack));

  forth_popda(); //drop flags
  return forth_popda();
}

void forth_execute_xt(uint32_t xt) {
  forth_pushda(xt);
  forth_pushda((uint32_t)execute);
  forth_core_fun_();
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
  forth_execute_xt(evaluate_xt);
}

void forth_eval_buf(char *s, bool verbose) {

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

#define MAX_LINE_LENGTH 128
#define NEWLINE 10

FIL eval_fil;
char eval_buf[MAX_LINE_LENGTH];

void forth_eval_file_or_die(const char *filename, bool verbose) {
  FRESULT fr;
  FILINFO fno;

  printf("Loading %s...\n", filename);

  fr = f_stat(filename, &fno);
  if (fr || (fno.fattrib & AM_DIR))
    die("File not found: %s.\n", filename);

  fr = f_open(&eval_fil, filename, FA_READ);
  assert(fr == 0);

  size_t len;
  char *line;

  while (!f_eof(&eval_fil)) {
    line = f_gets(eval_buf, MAX_LINE_LENGTH, &eval_fil);
    assert(line);
    len = strlen(line);

    if (line[len-1] != NEWLINE)
      die("line got truncated: %s.\n", line);

    if (verbose) printf("%s", line);

    // Skip the trailing newline.
    forth_evaluate(line, len - 1);
  }

  fr = f_close(&eval_fil);
  assert(fr == 0);
}


