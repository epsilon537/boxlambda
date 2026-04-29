#include "forth.h"
#include "stdio_redirect_ffi.h"
#include "stdio_stream.h"
#include <assert.h>

//
// This module sets up C stdio to Forth redirection
//

// Execution token of Words emit and key.
uint32_t emit_xt;
uint32_t key_xt;

// Forward C's getc() to Forth's key
static int forth_getc(FILE *file) {
  (void) file; /* Not used in this function */

  forth_execute_xt(key_xt);

  // Pop key's return value of the stack.
  return (int)forth_popda();
}

// Forward C's putc to Forth's emit.
static int forth_putc(char c, FILE *file) {
  (void) file; /* Not used in this function */

  // Push input character on the stack and call emit.
  forth_pushda((uint32_t)c);
  forth_execute_xt(emit_xt);

  return (int)c;
}

// A stdin stream object that forwards to Forth.
static FILE forth_stdin = FDEV_SETUP_STREAM(
 NULL,
 forth_getc,
 NULL,
 _FDEV_SETUP_READ);

// A stdout stream object that forwards to Forth.
static FILE forth_stdout = FDEV_SETUP_STREAM(
 forth_putc,
 NULL,
 NULL,
 _FDEV_SETUP_WRITE);

// Set up C stdio to Forth emit/key redirection.
void stdio_redirect_ffi_init() {
  // Initialize emit and key XTs
  emit_xt = forth_find_word("emit");
  assert(emit_xt);

  key_xt = forth_find_word("key");
  assert(key_xt);

  // Redirect picolibc's stdio to Forth stdio
  stdin_stream = forth_stdin;
  stdout_stream = forth_stdout;
}

