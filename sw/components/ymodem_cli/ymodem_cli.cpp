#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"
#include "ymodem.h"
#include <stdlib.h>
#include "ymodem_cli.h"

#define YMODEM_RX_BUF_SIZE_BYTES (4*1024*1024)

extern "C" {
  static void ymodem_rx(EmbeddedCli *cli, char *args, void *context) {
    uint32_t addr, buf_size, ymodem_rx_size;

    if (embeddedCliGetTokenCount(args) < 2) {
      addr = (uint32_t)malloc(YMODEM_RX_BUF_SIZE_BYTES);

      ymodem_rx_size = ymodem_receive((unsigned char*)addr, YMODEM_RX_BUF_SIZE_BYTES);

      addr = (uint32_t)realloc((void*)addr, ymodem_rx_size);
    }
    else {
      const char *addrString = embeddedCliGetToken(args, 1);
      const char *sizeString = embeddedCliGetToken(args, 2);

      sscanf(addrString, "%08X", &addr);
      sscanf(sizeString, "%d", &buf_size);

      ymodem_rx_size = ymodem_receive((unsigned char*)addr, buf_size);
    }

    printf("Ymodem received %d bytes into buffer at address 0x%x.\n", ymodem_rx_size, addr);
  }

  static void ymodem_tx(EmbeddedCli *cli, char *args, void *context) {

    if (embeddedCliGetTokenCount(args) < 3) {
      printf("Argument missing: ymodem_tx  <filename> <buf hex addr.> <buf. size in bytes>\n");
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

void add_ymodem_cli(EmbeddedCli* cli, struct uart* uart) {
  assert(cli);

  ymodem_init(uart);

  embeddedCliAddBinding(cli, {
        "ymodem_rx",          // command name (spaces are not allowed)
        "ymodem_rx [<buf. hex addr.> <buf. size>]: Ymodem rx into given buf.. Alloc. buffer if not provided.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        ymodem_rx               // binding function
  });

  embeddedCliAddBinding(cli, {
        "ymodem_tx",          // command name (spaces are not allowed)
        "ymodem_tx <filename> <hex address> <size_in_bytes>: Ymodem transmit given memory buffer with given filename",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        ymodem_tx               // binding function
  });
}

