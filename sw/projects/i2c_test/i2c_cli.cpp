#include "i2c_cli.h"
#include "i2c.h"
#include <assert.h>
#include <stdio.h>

extern "C" {
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
}

void add_i2c_cli(EmbeddedCli *cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "beginTransmission",          // command name (spaces are not allowed)
        "I2C beginTransmission <hex slaveAddr>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        beginTransmission               // binding function
  });

  embeddedCliAddBinding(cli, {
        "endTransmission",          // command name (spaces are not allowed)
        "I2C endTransmission",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        endTransmission               // binding function
  });

  embeddedCliAddBinding(cli, {
        "requestFrom",          // command name (spaces are not allowed)
        "I2C requestFrom <hex slaveAddr> <num. bytes>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        requestFrom        // binding function
  });

  embeddedCliAddBinding(cli, {
        "i2cread",          // command name (spaces are not allowed)
        "i2cread next byte",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        i2cread        // binding function
  });

  embeddedCliAddBinding(cli, {
        "i2cwrite",          // command name (spaces are not allowed)
        "i2cwrite <hex byte value>",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,           // optional pointer to any application context
        i2cwrite           // binding function
  });
}

