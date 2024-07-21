#include "cli.h"

#define EMBEDDED_CLI_IMPL
#include "embedded_cli.h"

#include "i2c.h"
#include <assert.h>
#include <stdio.h>

static struct uart *uartp = 0;

extern "C" {
  static void peekw(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) == 0) {
        printf("Argument missing: peekw <hex addr>\n");
    }
    else {
      uint32_t addr;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%08X", &addr);

      printf("peekw 0x%08X -> 0x%08X\n", addr, *(volatile unsigned *)addr);
    }
  }

  static void peekb(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) == 0) {
        printf("Argument missing: peekb <hex addr>\n");
    }
    else {
      uint32_t addr;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%08X", &addr);

      printf("peekb 0x%08X -> 0x%02X\n", addr, *(volatile unsigned char *)addr);
    }
  }

  static void pokew(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 2) {
        printf("Argument(s) missing: pokew <hex addr> <hex value>\n");
    }
    else {
      uint32_t addr, value;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%08X", &addr);

      token = embeddedCliGetTokenVariable(args, 2);
      sscanf(token, "%08X", &value);

      printf("pokew 0x%08X -> 0x%08X\n", addr, value);

      *(volatile uint32_t*)addr = value;
    }
  }

  static void pokeb(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 2) {
        printf("Argument(s) missing: pokeb <hex addr> <hex value>\n");
    }
    else {
      uint32_t addr;
      uint8_t value;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%08X", &addr);

      token = embeddedCliGetTokenVariable(args, 2);
      sscanf(token, "%02hhX", &value);

      printf("pokew 0x%08X -> 0x%08X\n", addr, (unsigned)value);

      *(volatile uint8_t*)addr = value;
    }
  }

  static void beginTransmission(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) == 0) {
        printf("Argument missing: beginTransmission <hex slave addr>\n");
    }
    else {
      uint8_t slaveAddr;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%02hhX", &slaveAddr);

      printf("beginTransmission 0x%x\n", (unsigned)slaveAddr);

      i2c.beginTransmission(slaveAddr);
    }
  }

  static void endTransmission(EmbeddedCli *cli, char *args, void *context) {
    uint8_t res = i2c.endTransmission();
    printf("endTransmission %s\n", (res == 0) ? "OK" : "Error");
  }

  static void requestFrom(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 2) {
        printf("Argument missing: requestFrom <hex slave addr> <num. bytes>\n");
    }
    else {
      uint8_t slaveAddr;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%02hhX", &slaveAddr);

      uint8_t numBytes;

      token = embeddedCliGetTokenVariable(args, 2);
      sscanf(token, "%02hhX", &numBytes);

      uint8_t res = i2c.requestFrom(slaveAddr, numBytes);
      printf("requestFrom 0x%x %d -> %s\n", (unsigned)slaveAddr, numBytes, (res == 0) ? "OK" : "Error");
    }
  }

  static void i2cread(EmbeddedCli *cli, char *args, void *context) {
    printf("i2cread -> 0x%x\n", i2c.read());
  }

  static void i2cwrite(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) == 0) {
        printf("Argument missing: i2cwrite <hex value>\n");
    }
    else {
      uint8_t value;

      char *token = embeddedCliGetTokenVariable(args, 1);
      sscanf(token, "%02hhX", &value);

      printf("i2cwrite 0x%x\n", (unsigned)value);

      i2c.write(value);
    }
  }

  static void writeChar(EmbeddedCli *embeddedCli, char c) {
    assert(uartp);

    while (!uart_tx_ready(uartp));
    uart_tx(uartp, (uint8_t)c);
  }
}

void cli(struct uart *uart) {
  assert(uart);

  printf("Starting CLI...\n");

  EmbeddedCliConfig *config = embeddedCliDefaultConfig();
  config->maxBindingCount = 16;

  uartp = uart;

  EmbeddedCli *cli = embeddedCliNew(config);
  cli->writeChar = writeChar;

  embeddedCliAddBinding(cli, {
        "peekw",          // command name (spaces are not allowed)
        "Peek word: peekw <hex addr>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        peekw               // binding function
  });

  embeddedCliAddBinding(cli, {
        "peekb",          // command name (spaces are not allowed)
        "Peek byte: peekb <hex addr>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        peekb               // binding function
  });

  embeddedCliAddBinding(cli, {
        "pokew",          // command name (spaces are not allowed)
        "Poke word: pokew <hex addr> <hex value>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        pokew               // binding function
  });

  embeddedCliAddBinding(cli, {
        "pokeb",          // command name (spaces are not allowed)
        "Poke byte: pokeb <hex addr> <hex value>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        pokeb               // binding function
  });

  embeddedCliAddBinding(cli, {
        "beginTransmission",          // command name (spaces are not allowed)
        "beginTransmission <hex slaveAddr>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        beginTransmission               // binding function
  });

  embeddedCliAddBinding(cli, {
        "endTransmission",          // command name (spaces are not allowed)
        "endTransmission",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        endTransmission               // binding function
  });

  embeddedCliAddBinding(cli, {
        "requestFrom",          // command name (spaces are not allowed)
        "requestFrom <hex slaveAddr> <num. bytes>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        requestFrom        // binding function
  });

  embeddedCliAddBinding(cli, {
        "i2cread",          // command name (spaces are not allowed)
        "i2cread",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        i2cread        // binding function
  });

  embeddedCliAddBinding(cli, {
        "i2cwrite",          // command name (spaces are not allowed)
        "i2cwrite <hex value>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        i2cwrite           // binding function
  });

  // provide all chars to cli
  while (1) {
    while (uart_rx_ready(uartp)) {
      embeddedCliReceiveChar(cli, (char)uart_rx(uartp));
    }

    embeddedCliProcess(cli);
  }
}

