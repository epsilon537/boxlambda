#include "forth.h"
#include "stdio_redirect_ffi.h"
#include "stdio_stream.h"
#include <assert.h>

uint32_t emit_xt;
uint32_t key_xt;

static int forth_getc(FILE *file) {
  (void) file; /* Not used in this function */

  forth_execute_xt(key_xt);

  return (int)forth_popda();
}

static int forth_putc(char c, FILE *file) {
  (void) file; /* Not used in this function */

  forth_pushda((uint32_t)c);
  forth_execute_xt(emit_xt);

  return (int)c;
}

static FILE forth_stdin = FDEV_SETUP_STREAM(
 NULL,
 forth_getc,
 NULL,
 _FDEV_SETUP_READ);

static FILE forth_stdout = FDEV_SETUP_STREAM(
 forth_putc,
 NULL,
 NULL,
 _FDEV_SETUP_WRITE);

void stdio_redirect_ffi_init() {
  emit_xt = forth_find_word("emit");
  assert(emit_xt);

  key_xt = forth_find_word("key");
  assert(key_xt);

  stdin_stream = forth_stdin;
  stdout_stream = forth_stdout;
}

