#define EMBEDDED_CLI_IMPL
#include "embedded_cli.h"
#include "embedded_cli_setup.h"
#include <assert.h>
#include <stdio.h>

static struct uart *uartp = 0;
static EmbeddedCli *cli = 0;

//Connect embedded-CLI output to our UART
static void writeChar(EmbeddedCli *embeddedCli, char c) {
  assert(uartp);

  //Wait until there's space...
  while (!uart_tx_ready(uartp));
  uart_tx(uartp, (uint8_t)c);
}

EmbeddedCli* createEmbeddedCli(struct uart *uart) {
  if (cli == 0) {
    EmbeddedCliConfig *config = embeddedCliDefaultConfig();
    config->maxBindingCount = 16;

    assert(uart);
    uartp = uart;

    cli = embeddedCliNew(config);
    cli->writeChar = writeChar;
  }

  return cli;
}

void embeddedCliStartLoop(void) {
  assert(cli);
  assert(uartp);

  // provide all input chars to cli
  while (1) {
    while (uart_rx_ready(uartp)) {
      embeddedCliReceiveChar(cli, (char)uart_rx(uartp));
    }

    embeddedCliProcess(cli);
  }
}

