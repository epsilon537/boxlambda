#include "peek_poke_cli.h"
#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"

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
}

void add_peek_poke_cli(EmbeddedCli* cli) {
  assert(cli);

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
}

