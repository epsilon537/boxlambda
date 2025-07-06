#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c.h"
#include "MCP79412RTC.h"
#include "sdram.h"

#include "rtcc_cli.h"
#include "i2c_cli.h"
#include "peek_poke_cli.h"
#include "embedded_cli_setup.h"

extern "C" {
  //_init is executed by picolibc startup code before main().
  void _init(void) {
    uart_set_baudrate(115200);
    mcycle_start();
  }

  //_exit is executed by the picolibc exit function.
  //An implementation has to be provided to be able to user assert().
  void	_exit (int status) {
    while (1);
  }
}

void rtcc_test(void) {
  /*Time elements structure*/
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

    if (!RTC.isRunning()) {
      printf("RTC oscillator failed to start. Aborting...\n");
      return;
    }
  }

  printf("Polling time in a loop. Push btn[0] to end loop and start CLI.\n");

  uint8_t prevSecond = 0;

  for (;;) {
    /*Exit function if btn 0 is pushed.*/
    if ((gpio_get_input() & 0x0100) != 0) {
      return;
    }

    RTC.read(tmElements);

    /*print a line when the seconds change.*/
    if (tmElements.Second != prevSecond) {
      prevSecond = tmElements.Second;

      printf("sec: 0x%x, min: 0x%x, hour: 0x%x.\n", (uint32_t)tmElements.Second, (uint32_t)tmElements.Minute, (uint32_t)tmElements.Hour);
    }
  }
}

int main(void) {
  printf("Delaying start by 1s...\n");
  usleep(1000000);
  printf("Starting...\n");

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  /* Run the RTCC test. After test we can fall through to CLI.*/
  rtcc_test();

  EmbeddedCli *cli = createEmbeddedCli();

  add_peek_poke_cli(cli);
  add_i2c_cli(cli);
  add_rtcc_cli(cli);

  embeddedCliStartLoop();
}

