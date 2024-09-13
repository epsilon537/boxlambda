#ifndef YMODEM_CLI_H
#define YMODEM_CLI_H

#include "embedded_cli.h"
#include "uart.h"

#ifdef __cplusplus
extern "C" {
#endif

/*Add ymodem commands to the CLI*/
void add_ymodem_cli(EmbeddedCli* cli, struct uart* uart);

#ifdef __cplusplus
}
#endif
#endif /*YMODEM_CLI_H*/
