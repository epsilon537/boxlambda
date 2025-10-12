#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

//
// Linker Variables:
//
extern uint32_t __forth_ram_start[];
extern uint32_t __forth_ram_end[];
extern uint32_t __forth_imem_start[];
extern uint32_t __forth_imem_end[];
extern uint32_t __datastack[];
extern uint32_t __datastack_size[];
extern uint32_t __datastack_end[];

//
// Forth Data Stack Accessor:
//
typedef struct {
  uint32_t tos;
  uint32_t *psp;
} Forth_Datastack;

extern Forth_Datastack datastack;

// There is no Return Stack Accessor.
// The call stack acts as return stack.

//
// These functions are implemented in forth.cpp.
//

void forth_init();
void forth_repl();

// Push a value onto the data stack
void forth_pushda(uint32_t val);

// Pop a value from the data stack
uint32_t forth_popda();

// Find a word and return its xt, or 0 if word is not found.
uint32_t forth_find_word(const char *s);

// Execute the given xt (looked up using forth_find_word).
void forth_execute_xt(uint32_t xt);

// A wrapper for the two previous actions. Look up a word,
// if found, executed it and return 0. If not found, return -1.
uint32_t forth_execute_word(const char *s);

// Evaluate the given string.
void forth_eval(const char *s);

// Evaluate the multi-line buffer pointed to by s line-by-line?
// If verbose flag is set, print each line as its being loaded.
void forth_load_buf(char *s, bool verbose);

// Register a C function so it can be called later from Forth.
#define forth_register_fun(fun, nargs, nrets, wordname) \
           forth_pushda((uint32_t)fun), forth_pushda(nargs), forth_pushda(nrets), forth_eval("c-fun " wordname)

#ifdef __cplusplus
}
#endif
