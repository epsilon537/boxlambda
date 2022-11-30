#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "sdram.h"
#include "memtest.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

int code_is_in_ddr(void) {
  uint32_t pc;

  //Retrieve the program counter.
  asm ("auipc %0, 0x0" : "=r" (pc));

  //Return true is pc is in ddr region.
  return (pc >= MAIN_RAM_BASE);
}

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  //GPIO1 bits3:0 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR)
    printf("This is a simulation.\n");
  else
    printf("This is not a simulation.\n");

   if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  unsigned long memtest_size = ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR) ? MEMTEST_SIZE_SIM : MEMTEST_SIZE_ARTY;

  printf("Memory Test through port 0:\n");

  if(!memtest((unsigned int *) MAIN_RAM_BASE, memtest_size)) {
    printf("Memory port 0 test failed!\n");
    while(1);
  }
  else {
    printf("Memory port 0 test successful.\n");
  }

  printf("Memory Test through port 1:\n");

  if(!memtest((unsigned int *) MAIN_RAM_BASE2, memtest_size)) {
    printf("Memory port 1 test failed!\n");
    while(1);
  }
  else {
    printf("Memory port 1 test successful.\n");
  }

  printf("DDR instruction access test:\n");

  //Copy code to DDR
  int (*fptr)(void) =  (int (*)(void))((int)(code_is_in_ddr) | MAIN_RAM_BASE);

  memcpy(fptr,
         code_is_in_ddr,
         ((char*)_init - (char*)code_is_in_ddr));

  if (fptr()) {
    printf("Successfully executed code from DDR.\n");
  }
  else {
    printf("Failed to execute code from DDR.\n");
    while(1);
  }

  printf("Test completed successfully.\n");

  for (;;) {
    gpio_set_output(&gpio0, leds);
    leds ^= 0xF;

    if ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR)
      usleep(500 * 10); //Sleep less when we're running inside a simulator.
    else
      usleep(500 * 1000);
  }
}
