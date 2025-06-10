#define EMBEDDED_CLI_IMPL
#include "embedded_cli.h"
#include "embedded_cli_setup.h"
#include <assert.h>
#include <stdio.h>

static EmbeddedCli *cli = 0;

//Connect embedded-CLI output to our UART
static void writeChar(EmbeddedCli *embeddedCli, char c) {
  //Wait until there's space...
  while (!uart_tx_ready());
  uart_tx((uint8_t)c);
}

EmbeddedCli* createEmbeddedCli(void) {
  if (cli == 0) {
    EmbeddedCliConfig *config = embeddedCliDefaultConfig();
    config->maxBindingCount = 24;

    cli = embeddedCliNew(config);
    cli->writeChar = writeChar;
  }

  return cli;
}

void embeddedCliStartLoop(void) {
  assert(cli);

  // provide all input chars to cli
  while (1) {
    while (uart_rx_ready()) {
      embeddedCliReceiveChar(cli, (char)uart_rx());
    }

    embeddedCliProcess(cli);
  }
}

