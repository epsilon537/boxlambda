#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "sdram.h"
#include "flashdrvr.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

#ifdef __cplusplus
extern "C"
{
#endif
//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);

  mtime_start();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}
#ifdef __cplusplus
}
#endif

int main(void) {
  //Switches
  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  //Buttons
  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  printf("Starting test...\n");

  {
    static FLASHDRVR flashdrvr;

    printf("Reading one byte from FLASHBASE+0x800000:\n");
    volatile char x = *(volatile char *)(FLASHBASE+0x800000);
    printf("Read back value = 0x%x\n", x);

    static const int TEST_STR_LEN=13;
    static const char testStr[TEST_STR_LEN] = "Hello World.";

    printf("Writing to FLASHBASE+0x800000:\n");
    flashdrvr.write(FLASHBASE+0x800000, TEST_STR_LEN, testStr);

    for (int ii=0; ii<TEST_STR_LEN; ++ii) {
      printf("Written [%d]: 0x%x\n", ii, testStr[ii]);
    }

    printf("Reading back from FLASHBASE+0x800000:\n");

    static char readbackStr[TEST_STR_LEN+1] = "             ";
    memcpy(readbackStr, (const char*)(FLASHBASE+0x800000), TEST_STR_LEN);

    for (int ii=0; ii<TEST_STR_LEN; ++ii) {
      printf("Read back [%d]: 0x%x\n", ii, readbackStr[ii]);
    }

    memcpy(readbackStr, (const char*)(FLASHBASE+0x800000), TEST_STR_LEN);

    for (int ii=0; ii<TEST_STR_LEN; ++ii) {
      printf("Read back [%d]: 0x%x\n", ii, readbackStr[ii]);
    }

    if (strncmp(readbackStr, testStr, TEST_STR_LEN) == 0) {
      printf("Test Successful.\n");
    } else {
      printf("Strings don't match!\n");
      return -1;
    }
  }

  return 0;
}
