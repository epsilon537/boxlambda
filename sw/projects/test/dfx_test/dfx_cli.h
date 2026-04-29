#ifndef DFX_CLI_H
#define DFX_CLI_H

#include "embedded_cli.h"

#ifdef __cplusplus
extern "C" {
#endif

/*Add DFX commands to the CLI*/
void add_dfx_cli(EmbeddedCli* cli);

#ifdef __cplusplus
}
#endif
#endif /*DFX_CLI_H*/
