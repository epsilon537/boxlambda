/* This is an embedded-CLI client component defining CLI commandis to transfer files to
 * and from BoxLambda using the ymodem protocol. */

#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"
#include "ymodem.h"
#include <stdlib.h>
#include "ymodem_cli.h"
#include "ff.h"

#define YMODEM_RX_BUF_SIZE_BYTES (4*1024*1024)

extern "C" {
  //CLI command to receive a file via ymodem.
  static void ymodem_rx(EmbeddedCli *cli, char *args, void *context) {
    uint32_t addr, buf_size, ymodem_rx_size;

    if (embeddedCliGetTokenCount(args) < 1) {
      printf("Argument missing: ymodem_rx <filename>\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);

      addr = (uint32_t)malloc(YMODEM_RX_BUF_SIZE_BYTES);

      ymodem_rx_size = ymodem_receive((unsigned char*)addr, YMODEM_RX_BUF_SIZE_BYTES);

      printf("Ymodem received %d bytes into buffer at address 0x%x.\n", ymodem_rx_size, addr);

      if (ymodem_rx_size != 0) {
        printf("Saving received modem_data to file %s.\n", filename);

        FIL file_object;

        /* Create a new file */
        FRESULT res = f_open(&file_object, (char const *)filename, FA_CREATE_ALWAYS | FA_WRITE);
        if (res != FR_OK) {
          printf("FatFS file open error!\n");
          return;
        }

        UINT byte_written;
        res = f_write(&file_object, (const void*)addr, ymodem_rx_size, &byte_written);

        if (res != FR_OK) {
          printf("FatFS file write error!\n");
           return;
        }

        f_close(&file_object);
      }

      free((void*)addr);
    }
  }

  //CLI command to send a file via ymodem.
  static void ymodem_tx_buf(EmbeddedCli *cli, char *args, void *context) {

    if (embeddedCliGetTokenCount(args) < 3) {
      printf("Argument missing: ymodem_tx_buf  <filename> <buf hex addr.> <buf. size in bytes>\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);
      const char *addrString = embeddedCliGetToken(args, 2);
      const char *sizeString = embeddedCliGetToken(args, 3);
      uint32_t addr, buf_size;

      sscanf(addrString, "%08X", &addr);
      sscanf(sizeString, "%d", &buf_size);

      ymodem_send((unsigned char*)addr, buf_size, (char*)filename);

      printf("Ymodem transmitted %d bytes into buffer at address 0x%x to file %s.\n", buf_size, addr, filename);
    }
  }
}

/* Call this function to hook the ymodem CLI commanded into the embedded-cli */
/* instance running on the system. */
void add_ymodem_cli(EmbeddedCli* cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "ymodem_rx",          // command name (spaces are not allowed)
        "ymodem_rx <filename>: Ymodem rx and save to give file.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        ymodem_rx               // binding function
  });

  embeddedCliAddBinding(cli, {
        "ymodem_tx_buf",          // command name (spaces are not allowed)
        "ymodem_tx_buf <filename> <hex address> <size_in_bytes>: Ymodem transmit given memory buffer with given filename",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        ymodem_tx_buf               // binding function
  });
}

