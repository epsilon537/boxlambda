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
#include "ym2149_sys_regs.h"
#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

void ym2149_sys_reg_wr(unsigned reg_offset, unsigned val) {
  ((unsigned volatile*)(YM2149_SYS_BASE))[reg_offset] = val;
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

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  printf("YM2149 test.\n");

  unsigned addrs[] = {  0,  1,  2,  3,    4, 5,  6,    7 ,  8,  9, 10, 11, 12, 13,    16  , 17,   18, 19,   20, 21, 22,   23, 24, 25, 26, 27, 28, 29, 128,129, 130,131,132,133, 134, 135, 136, 137 } ;
  unsigned vals[]  = { 0x1c,1,0xfd, 0, 0xef, 0,  0, 0xf8 , 15,  15, 15,  1,  0, 13,   0xd5,  0, 0xbe,  0, 0xb3,  0,  0, 0xf8, 15, 15, 15,  1,  0, 13,  64, 64,  64, 64, 64, 64, 128,   0,  25, 128 } ;

  for (int ii=0; ii<(sizeof(addrs)/sizeof(addrs[0])); ii++) {
    ym2149_sys_reg_wr(addrs[ii], vals[ii]);
  }

  printf("YM2149 config complete.\n");

  while(1);

  return 0;
}
