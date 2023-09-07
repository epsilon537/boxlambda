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
#include "praxos_wordcopy_prog.h"
#include "praxos_regs.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

static unsigned long long praxos_wordcopy_prog[] = PRAXOS_WORDCOPY_PROG;

static unsigned srcBuf[32], dstBuf[32];

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
  //Switches
  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  //Buttons
  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  printf("Load Praxos Program...\n");

  //Load the program into memory
  for (int ii=0; ii<sizeof(praxos_wordcopy_prog)/sizeof(praxos_wordcopy_prog[0]); ii++) {
    praxos_sys_reg_wr(PRAXOS_PM_DATA_LO, (unsigned)praxos_wordcopy_prog[ii]);
    praxos_sys_reg_wr(PRAXOS_PM_DATA_HI, (unsigned)(praxos_wordcopy_prog[ii]>>32));
    praxos_sys_reg_wr(PRAXOS_PM_ADDR, ii);
    praxos_sys_reg_wr(PRAXOS_PM_WR, 0);
  }

  printf("Taking Praxos out of reset...\n");
  praxos_sys_reg_wr(PRAXOS_CTRL, 1);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcBuf[ii] = (unsigned)rand();
    dstBuf[ii] = 0;
  }

  printf("Configuring DMA request.\n");
  printf("numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", 32, (unsigned)srcBuf, (unsigned)dstBuf);

  praxos_gp_reg_wr(0, (unsigned)srcBuf);
  praxos_gp_reg_wr(1, (unsigned)dstBuf);
  praxos_gp_reg_wr(2, 32);

  printf("Kicking off DMA...\n");
  praxos_gp_reg_wr(3, 1);
    
  printf("Waiting for completion...\n");
  int dmaBusy = 1;

  while(dmaBusy) {
    dmaBusy = praxos_gp_reg_rd(3);
  }

  printf("Checking result...\n");
  if (!memcmp(srcBuf, dstBuf, 32*sizeof(unsigned))) {
    printf("Praxos Wordcopy test successful.\n");
  }
  else {
    printf("Praxos Wordcopy test failed.\n");
  }

  return 0;
}
