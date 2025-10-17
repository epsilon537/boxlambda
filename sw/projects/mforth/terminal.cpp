#include "terminal.h"
#include "forth.h"

// Forth word XTs
uint32_t emit_xt;
uint32_t key_xt;
uint32_t qemit_xt;
uint32_t qkey_xt;

void TERMINAL_init() {
 emit_xt = forth_find_word("emit");
 key_xt = forth_find_word("key");
 qemit_xt = forth_find_word("emit?");
 qkey_xt = forth_find_word("key?");
}

void TERMINAL_emit(char c) {
  forth_pushda((uint32_t)c);
  forth_execute_xt(emit_xt);
}

void TERMINAL_key(char *c) {
  forth_execute_xt(key_xt);
  *c = (char)forth_popda();
}

void TERMINAL_qemit(char *c) {
  forth_execute_xt(qemit_xt);
  *c = (char)forth_popda();
}

void TERMINAL_qkey(char *c) {
  forth_execute_xt(qkey_xt);
  *c = (char)forth_popda();
}

void TERMINAL_redirect(void) {
}

void TERMINAL_unredirect(void) {
}


