#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c.h"
#include "MCP79412RTC.h"
#include "sdram.h"

#define EMBEDDED_CLI_IMPL
#include "embedded_cli.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.
#define TEST_STRING_SIZE 16

#define I2C_SLAVE_ADDR 0x6F
#define I2C_SLAVE_SRAM_START_ADDR 0x20

#define I2C_SPEED_SIM 40
#define I2C_SPEED_FPGA 1000

static struct uart uart0;
static struct gpio gpio;

extern "C" {
  //_init is executed by picolibc startup code before main().
  void _init(void) {
    //Set up UART and tie stdio to it.
    uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
    uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
    set_stdio_to_uart(&uart0);
    mcycle_start();
  }

  //_exit is executed by the picolibc exit function.
  //An implementation has to be provided to be able to user assert().
  void	_exit (int status) {
    while (1);
  }

  void writeChar(EmbeddedCli *embeddedCli, char c) {
    while (!uart_tx_ready(&uart0));
    uart_tx(&uart0, (uint8_t)c);
  }

  void peekw(EmbeddedCli *cli, char *args, void *context) {
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

  void peekb(EmbeddedCli *cli, char *args, void *context) {
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

  void pokew(EmbeddedCli *cli, char *args, void *context) {
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

  void pokeb(EmbeddedCli *cli, char *args, void *context) {
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

  void beginTransmission(EmbeddedCli *cli, char *args, void *context) {
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

  void endTransmission(EmbeddedCli *cli, char *args, void *context) {
    printf("endTransmission\n");

    i2c.endTransmission();
  }

  void requestFrom(EmbeddedCli *cli, char *args, void *context) {
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
      printf("requestFrom 0x%x %d\n", (unsigned)slaveAddr, numBytes);

      i2c.requestFrom(slaveAddr, numBytes);
    }
  }

  void i2cread(EmbeddedCli *cli, char *args, void *context) {
    printf("i2cread -> 0x%x\n", i2c.read());
  }

  void i2cwrite(EmbeddedCli *cli, char *args, void *context) {
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

void rtcc_test(void) {
  tmElements_t tmElements;
  /*Some bogus initial values.*/
  tmElements.Second = 1;
  tmElements.Minute = 2;
  tmElements.Hour = 3;
  tmElements.Wday = 4;   // day of week, sunday is day 1
  tmElements.Day = 5;
  tmElements.Month = 6;
  tmElements.Year = 7;   // offset from 1970;

  RTC.begin();

  if (RTC.isRunning()) {
    printf("RTC oscillator is already running.\n");
  }
  else {
    printf("RTC oscillator is not running yet. Enabling...\n");

    RTC.vbaten(true);
    RTC.write(tmElements);
  }

  if (!RTC.isRunning()) {
    printf("RTC is not running. Aborting...\n");
    return;
  }

  printf("Polling time in a loop. Push btn[0] to end loop and start CLI.\n");

  uint8_t prevSecond = 0;

  for (;;) {
    /*Exit function if btn 0 is pushed.*/
    if ((gpio_get_input(&gpio) & 0x0100) != 0) {
      return;
    }

    RTC.read(tmElements);

    /*print a line when the seconds change.*/
    if (tmElements.Second != prevSecond) {
      prevSecond = tmElements.Second;

      printf("sec: 0x%x, min: 0x%x, hour: 0x%x.\n", (unsigned)tmElements.Second, (unsigned)tmElements.Minute, (unsigned)tmElements.Hour);
    }
  }
}

int main(void) {
  uint32_t leds = 0xF;

  printf("Delaying start 1s...\n");
  usleep(1000000);
  printf("Starting...\n");

  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  /* Run the RTCC test. After test we can fall through to CLI.*/
  rtcc_test();

  /*If we get here, the test fell through unsuccessfully.*/

  printf("Starting CLI...\n");

  EmbeddedCliConfig *config = embeddedCliDefaultConfig();
  config->maxBindingCount = 16;

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
    while (uart_rx_ready(&uart0)) {
      embeddedCliReceiveChar(cli, (char)uart_rx(&uart0));
    }

    embeddedCliProcess(cli);
  }
}

