#include "j1b_cli.h"
#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"
#include "j1b_hal.h"
#include "j1b_nuc.h"

static unsigned j1b_nuc_prg[] = J1B_NUC_PRG;
static struct uart* uartp = 0;

extern "C" {
  static void j1b_boot(EmbeddedCli *cli, char *args, void *context) {
    printf("Reading core signature register...\n");

    uint32_t sig = j1b_reg_rd(J1B_REG_SIGNATURE);

    printf("Read signature value: 0x%x\n", sig);

    if (sig != J1B_SIG_VALUE) {
      printf("Incorrect signature! Expected 0x%x. Aborting...\n", J1B_SIG_VALUE);
      return;
    }

    printf("Signature correct.\n");

    printf("J1B program length in bytes: %d\n", sizeof(j1b_nuc_prg));

    j1b_load_program((unsigned char*)j1b_nuc_prg, sizeof(j1b_nuc_prg));

    printf("Taking J1B out of reset...\n");

    j1b_reg_wr(J1B_REG_CTRL, J1B_REG_CTRL_RST_N);

    printf("Done.\n");
  }

  static void j1b_fwd_uart(EmbeddedCli *cli, char *args, void *context) {
    unsigned socUartRx;
    unsigned j1bUartRx;

    printf("Forwarding UART...\n");

    for (;;) {
      if (uart_rx_ready(uartp)) {
        socUartRx = (char)uart_rx(uartp);

        while(j1b_reg_rd(J1B_REG_UART_TX_TO_J1B) & J1B_REG_UART_TX_TO_J1B_DATA_WAITING);

        j1b_reg_wr(J1B_REG_UART_TX_TO_J1B, socUartRx);
      }

      j1bUartRx = j1b_reg_rd(J1B_REG_UART_RX_FROM_J1B);
      if (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_DATA_AVL) {
         while(!uart_tx_ready(uartp));
         uart_tx(uartp, (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_RX_DATA_MSK));
      }
    }
  }
}

void add_j1b_cli(EmbeddedCli* cli, struct uart* uart) {
  assert(cli);
  assert(uart);

  uartp = uart;

  embeddedCliAddBinding(cli, {
        "j1b_boot",          // command name (spaces are not allowed)
        "Boot J1B core.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        j1b_boot               // binding function
  });

  embeddedCliAddBinding(cli, {
        "j1b_fwd_uart",          // command name (spaces are not allowed)
        "Forward UART I/O to J1B.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        j1b_fwd_uart               // binding function
  });
}

