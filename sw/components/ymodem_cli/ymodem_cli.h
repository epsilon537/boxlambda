#ifndef YMODEM_CLI_H
#define YMODEM_CLI_H

#include "embedded_cli.h"

#ifdef __cplusplus
extern "C" {
#endif

/*Add ymodem commands to the CLI*/
void add_ymodem_cli(EmbeddedCli* cli);

#ifdef __cplusplus
}
#endif
#endif /*YMODEM_CLI_H*/
