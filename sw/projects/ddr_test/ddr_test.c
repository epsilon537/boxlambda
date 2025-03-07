#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdram.h"
#include "libbase/memtest.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.

static struct uart uart0;

static struct gpio gpio;

/*Making CRC variable of code_in_ddr() test routine volatile to make sure we also generate some data accesses
 *to internal memory while executing from DDR.*/
static volatile unsigned int crc;

/*This function will be copied to DDR memory.
 *It generates a mix of DDR data and instruction accesses by computing
 *a CRC over a chunk of DDR memory.*/
int code_in_ddr(char *message) {
  int i, j;
  unsigned int byte, mask;

   i = 0;
   crc = 0xFFFFFFFF;
   while (i<16) {
      byte = message[i];            // Get next byte.
      crc = crc ^ byte;
      for (j = 7; j >= 0; j--) {    // Do eight times.
         mask = -(crc & 1);
         crc = (crc >> 1) ^ (0xEDB88320 & mask);
      }
      i = i + 1;
   }

   return crc;
}

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

//Adding this specific test because this used to fail.
static int doubleWriteTest() {
  void *dst = malloc(sizeof(unsigned));
  unsigned val1 = 0xffffffff;
  unsigned val2 = 0x11111111;

  asm inline volatile (
    "sw %1, 0(%0) \n\t"
    "sw %2, 0(%0)"
    :
    : "r" (dst), "r" (val1), "r" (val2));

  unsigned res = *(volatile int *)dst;

  free(dst);

  if (res != val2) {
    printf("Double write test failed: Read: 0x%x, expected 0x%x\n", res, val2);
    return 0;
  }
  else {
    printf("Double write test OK.\n");
    return 1;
  }
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  //GPIO bits 7:4 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio) & 0xf0) == GPIO_SIM_INDICATOR)
    printf("This is a simulation.\n");
  else
    printf("This is not a simulation.\n");

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  if (doubleWriteTest() == 0) {
    printf("Test failed.\n");
    while (1);
  }

  unsigned long memtest_size = MEMTEST_SIZE;

  printf("Memory Test:\n");

  if(!memtest((unsigned int *) MAIN_RAM_BASE, memtest_size)) {
    printf("Memory test failed!\n");
    while(1);
  }
  else {
    printf("Memory test successful.\n");
  }

  printf("DDR instruction access test:\n");

  //Copy code to DDR
  int (*fptr)(char*);
  fptr =  (int (*)(char*))((int)(code_in_ddr) | MAIN_RAM_BASE);
  memcpy(fptr,
         code_in_ddr,
         32 + ((char*)_init - (char*)code_in_ddr));
  /*Execute the code in DDR*/
  int ext_mem_crc = fptr((char*)fptr);

  /*Execute again in internal memory*/
  int int_mem_crc = code_in_ddr((char*)code_in_ddr);

  if (ext_mem_crc == int_mem_crc) {
    printf("Successfully executed code from DDR.\n");
  }
  else {
    printf("DDR code execution test failed!\n");
    while(1);
  }

  printf("Test Successful.\n");

  for (;;) {
    gpio_set_output(&gpio, leds);
    leds ^= 0xF;

    if ((gpio_get_input(&gpio) & 0xf0) == GPIO_SIM_INDICATOR)
      usleep(500 * 10); //Sleep less when we're running inside a simulator.
    else
      usleep(500 * 1000);
  }
}
