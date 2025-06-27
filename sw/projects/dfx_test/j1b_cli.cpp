#include "j1b_cli.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "embedded_cli.h"
#include "vs0_hal.h"
#include "j1b_hal.h"
#include "ff.h"
#include "uart.h"

//The ASCII code for <ESC>
#define ESC 27

extern "C" {
  /* CLI command to load a J1B firmware image from disk and use it to boot the J1B core. */
  static void j1b_boot(EmbeddedCli *cli, char *args, void *context) {
    uint16_t tokenCount = embeddedCliGetTokenCount(args);

    if (tokenCount < 1) {
        printf("Argument missing: j1b_boot <filename>\n");
    }
    else {
      printf("Reading core signature register...\n");

      uint32_t sig = vs0_reg_rd(VS0_REG_SIGNATURE);

      printf("Read signature value: 0x%x\n", sig);

      if (sig != J1B_SIG_VALUE) {
        printf("Incorrect signature! Expected 0x%x. RM vs0_j1b is not DFX loaded currently. Aborting...\n", J1B_SIG_VALUE);
        return;
      }

      printf("Signature correct.\n");

      const char *filename = embeddedCliGetToken(args, 1);
	    FIL file_object;
      uint32_t addr;
      uint32_t size;

      /* Open the file */
      FRESULT res = f_open(&file_object, (char const *)filename, FA_OPEN_EXISTING | FA_READ);
      if (res != FR_OK) {
        printf("FatFS file open error! Error code: %d\n", res);
        return;
      }

      size = f_size(&file_object);
      addr = (uint32_t)malloc(size);
      printf("Loading file %s, size: %d bytes, into memory at address 0x%x.\n", filename, size, addr);

      /* Read file */
      UINT bytes_read;
      res = f_read(&file_object, (void*)addr, size, &bytes_read);
      if (res != FR_OK) {
        printf("FatFS file read error! Error code: %d\n", res);
        return;
      }

      /* Close the file*/
      f_close(&file_object);

      printf("Booting J1B...\n");

      //Load the program into the J1B's program memory.
      j1b_load_program((uint8_t*)addr, size);

      printf("Taking J1B out of reset...\n");

      j1b_reg_wr(J1B_REG_CTRL, J1B_REG_CTRL_RST_N);

      printf("Done.\n");
    }
  }

  //CLI command to forward all interaction on the serial port console to the J1B core.
  static void j1b_fwd_uart(EmbeddedCli *cli, char *args, void *context) {
    printf("Reading core signature register...\n");

    uint32_t sig = vs0_reg_rd(VS0_REG_SIGNATURE);

    printf("Read signature value: 0x%x\n", sig);

    if (sig != J1B_SIG_VALUE) {
      printf("Incorrect signature! Expected 0x%x. RM vs0_j1b is not DFX loaded currently. Aborting...\n", J1B_SIG_VALUE);
      return;
    }

    printf("Signature correct. Checking if J1B has been booted up...\n");

    if ((j1b_reg_rd(J1B_REG_CTRL) & J1B_REG_CTRL_RST_N) == 0) {
      printf("J1B hasn't been booted up yet. Aborting...\n");
      return;
    }

    uint32_t socUartRx;
    uint32_t j1bUartRx;

    printf("Forwarding UART. Press <ESC> to return.\n");


    for (;;) {
      //Check if a character is waiting in the UART (does not block).
      if (uart_rx_ready()) {
        socUartRx = (char)uart_rx();

        if (socUartRx == ESC) {
          printf("Returning to shell...\n");
          break;
        }

        //Wait until there's space for a character in the J1B UART received direction.
        while(j1b_reg_rd(J1B_REG_UART_TX_TO_J1B) & J1B_REG_UART_TX_TO_J1B_DATA_WAITING);

        j1b_reg_wr(J1B_REG_UART_TX_TO_J1B, socUartRx);
      }

      //Check for output waiting in the J1B UART transmit direction.
      j1bUartRx = j1b_reg_rd(J1B_REG_UART_RX_FROM_J1B);
      if (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_DATA_AVL) {
         while(!uart_tx_ready());
         uart_tx((j1bUartRx&J1B_REG_UART_RX_FROM_J1B_RX_DATA_MSK));
      }
    }
  }
}

//Call this function to hook the above functions into the embedded-CLI instance running on the system.
void add_j1b_cli(EmbeddedCli* cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "j1b_boot",          // command name (spaces are not allowed)
        "j1b_boot <filename>: Boot J1B core with given FW image.",   // Optional help for a command (NULL for no help)
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

