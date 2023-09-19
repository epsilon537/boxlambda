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
#include "picorv_wordcopy.h"
#include "picorv_bytecopy.h"
#include "picorv_dma_hal.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

static unsigned srcBuf[32], dstBuf[32];
static unsigned char srcBufBytes[36], dstBufBytes[36];

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

  printf("Load PicoRV Program...\n");

  picorv_load_program(picorv_wordcopy_picobin, picorv_wordcopy_picobin_len);

  printf("Taking PicoRV out of reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcBuf[ii] = (unsigned)rand();
    dstBuf[ii] = 0;
  }

  printf("Configuring DMA request.\n");
  printf("numWords = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", 32, (unsigned)srcBuf, (unsigned)dstBuf);

  picorv_gp_reg_wr(0, (unsigned)srcBuf);
  picorv_gp_reg_wr(1, (unsigned)dstBuf);
  picorv_gp_reg_wr(2, 32);

  printf("Kicking off DMA...\n");
  picorv_gp_reg_wr(3, 1);
    
  printf("Waiting for completion...\n");
  int dmaBusy = 1;

  while(dmaBusy) {
    dmaBusy = picorv_gp_reg_rd(3);
  }

  printf("Checking result...\n");
  if (!memcmp(srcBuf, dstBuf, 32*sizeof(unsigned))) {
    printf("PicoRV Wordcopy test successful.\n");
  }
  else {
    printf("PicoRV Wordcopy test failed.\n");
    return 0;
  }

  printf("Putting PicoRV back into reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 0);

  printf("Load PicoRV Program...\n");

  picorv_load_program(picorv_bytecopy_picobin, picorv_bytecopy_picobin_len);

  printf("Taking PicoRV out of reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<36; ii++) {
    srcBufBytes[ii] = (unsigned char)rand();
    dstBufBytes[ii] = 0;
  }

  unsigned char* srcPtr = srcBufBytes + 1;
  unsigned char* dstPtr = dstBufBytes + 3;

  printf("Configuring DMA request.\n");
  printf("numBytes = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", 32, (unsigned)(srcPtr), (unsigned)(dstPtr));

  picorv_gp_reg_wr(0, (unsigned)srcPtr);
  picorv_gp_reg_wr(1, (unsigned)dstPtr);
  picorv_gp_reg_wr(2, 32);

  printf("Kicking off DMA...\n");
  picorv_gp_reg_wr(3, 1);
    
  printf("Waiting for completion...\n");
  dmaBusy = 1;

  while(dmaBusy) {
    dmaBusy = picorv_gp_reg_rd(3);
  }

  printf("Checking result...\n");
  if (!memcmp(srcPtr, dstPtr, 32)) {
    printf("PicoRV Bytecopy test successful.\n");
  }
  else {
    printf("PicoRV Bytecopy test failed.\n");
    return 0;
  }

  printf("PicoRV Word and Bytecopy test successful.\n");
  
  return 0;
}
