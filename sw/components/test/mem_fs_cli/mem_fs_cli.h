#ifndef MEM_FS_CLI_H
#define MEM_FS_CLI_H

#include "embedded_cli.h"

#ifdef __cplusplus
extern "C" {
#endif

/*Add memory/filesystem commands to the CLI*/
void add_mem_fs_cli(EmbeddedCli* cli);

#ifdef __cplusplus
}
#endif
#endif /*MEM_FS_CLI_H*/
