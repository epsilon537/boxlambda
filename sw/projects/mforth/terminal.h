/*
 * terminal.h - BoxLambda version by Ruben Lysens / Epsilon537
 *
 * Original author: psi
 */

#ifndef INC_TERMINAL_H_
#define INC_TERMINAL_H_

void TERMINAL_init();
void TERMINAL_emit(char c);
void TERMINAL_key(char *c);
void TERMINAL_qemit(char *c);
void TERMINAL_qkey(char *c);

void TERMINAL_redirect(void);
void TERMINAL_unredirect(void);


#endif /* INC_TERMINAL_H_ */
