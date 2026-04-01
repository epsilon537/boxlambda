#ifndef EMBEDDED_CLI_SETUP_H
#define EMBEDDED_CLI_SETUP_H

#include "embedded_cli.h"

#ifdef __cplusplus
extern "C" {
#endif

/*i Create an EmbeddedCli object connected to the UART, ready to be used*/
EmbeddedCli* createEmbeddedCli(void);

/* Enter the CLI loop. Assumes createEmbeddedCli has been called and the CLI object has been
 * populated with CLI commands. This function does not return.*/
void embeddedCliStartLoop(void);

#ifdef __cplusplus
}
#endif
#endif /*EMBEDDED_CLI_SETUP_H*/
