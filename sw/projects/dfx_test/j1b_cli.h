#ifndef J1B_CLI_H
#define J1B_CLI_H

#include "embedded_cli.h"
#include "uart.h"

#ifdef __cplusplus
extern "C" {
#endif

/*Add J1B commands to the CLI*/
void add_j1b_cli(EmbeddedCli* cli, struct uart* uart);

#ifdef __cplusplus
}
#endif
#endif /*J1B_CLI_H*/
