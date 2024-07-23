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

#include "i2c_cli.h"

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

  printf("Delaying start by 1s...\n");
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
  cli(&uart0);
}

