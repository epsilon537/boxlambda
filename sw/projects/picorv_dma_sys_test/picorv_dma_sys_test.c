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
//PicoRV wordcopy program
#include "picorv_wordcopy_single.h"
//PicoRV bytecopy program
#include "picorv_bytecopy_single.h"
#include "picorv_dma_hal.h"
#include "sdram.h"
#include "vera_hal.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

#define PICORV_HIR_REG_SRC PICORV_HIR_0
#define PICORV_HIR_REG_DST PICORV_HIR_1
#define PICORV_HIR_REG_NUM_ELEMS PICORV_HIR_2
#define PICORV_HIR_REG_CTRL_STAT PICORV_HIR_3

#define DMA_START 1
#define DMA_BUSY 2

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

//Some data bufers to copy from/to.
static unsigned srcBufWords[32], dstBufWords[32];
static unsigned char srcBufBytes[36], dstBufBytes[36];

unsigned char* srcPtrBytes;
unsigned char* dstPtrBytes;
unsigned* srcPtrWords;
unsigned* dstPtrWords;

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

//Copy <numElems> elements from <src> address to dst address.
//An element is a word or a byte depending on whether the wordcopy or the bytecopy program
//is installed into PicoRV.
static void dma_copy(void* src, void* dst, unsigned numElems) {
  printf("Configuring DMA request.\n");
  printf("numElems = %d, srcAddr = 0x%x, dstAddr = 0x%x\n", numElems, (unsigned)src, (unsigned)dst);

  picorv_hir_reg_wr(PICORV_HIR_REG_SRC, (unsigned)src);
  picorv_hir_reg_wr(PICORV_HIR_REG_DST, (unsigned)dst);
  picorv_hir_reg_wr(PICORV_HIR_REG_NUM_ELEMS, numElems);

  printf("Kicking off DMA...\n");
  picorv_hir_reg_wr(PICORV_HIR_REG_CTRL_STAT, DMA_START);
    
  printf("Waiting for completion...\n");
  int dmaBusy = DMA_BUSY;
  while(dmaBusy) {
    dmaBusy = picorv_hir_reg_rd(PICORV_HIR_REG_CTRL_STAT);
  }
}

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

  printf("Load PicoRV WordCopy Program...\n");

  picorv_load_program(picorv_wordcopy_single_picobin, picorv_wordcopy_single_picobin_len);

  printf("Taking PicoRV out of reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

  printf("Internal memory wordcopy test\n");

  srcPtrWords = srcBufWords;
  dstPtrWords = dstBufWords;

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrWords[ii] = (unsigned)rand();
    dstPtrWords[ii] = 0;
  }

  dma_copy(srcPtrWords, dstPtrWords, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrWords, dstPtrWords, 32*sizeof(unsigned))) {
    printf("PicoRV Wordcopy test successful.\n");
  }
  else {
    printf("PicoRV Wordcopy test failed.\n");
    return 0;
  }

  printf("External memory wordcopy test, port 0\n");
  srcPtrWords = (unsigned*)MAIN_RAM_BASE;
  dstPtrWords = (unsigned*)(MAIN_RAM_BASE + 1024);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrWords[ii] = (unsigned)rand();
    dstPtrWords[ii] = 0;
  }

  dma_copy(srcPtrWords, dstPtrWords, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrWords, dstPtrWords, 32*sizeof(unsigned))) {
    printf("PicoRV Wordcopy test successful.\n");
  }
  else {
    printf("PicoRV Wordcopy test failed.\n");
    return 0;
  }

  printf("External memory wordcopy test, port 1\n");
  srcPtrWords = (unsigned*)MAIN_RAM_BASE2;
  dstPtrWords = (unsigned*)(MAIN_RAM_BASE2 + 1024);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrWords[ii] = (unsigned)rand();
    dstPtrWords[ii] = 0;
  }

  dma_copy(srcPtrWords, dstPtrWords, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrWords, dstPtrWords, 32*sizeof(unsigned))) {
    printf("PicoRV Wordcopy test successful.\n");
  }
  else {
    printf("PicoRV Wordcopy test failed.\n");
    return 0;
  }

  printf("External memory wordcopy to VRAM test\n");
  srcPtrWords = (unsigned*)MAIN_RAM_BASE;
  dstPtrWords = (unsigned*)VERA_VRAM_BASE;

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrWords[ii] = (unsigned)rand();
    dstPtrWords[ii] = 0;
  }

  dma_copy(srcPtrWords, dstPtrWords, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrWords, dstPtrWords, 32*sizeof(unsigned))) {
    printf("PicoRV Wordcopy test successful.\n");
  }
  else {
    printf("PicoRV Wordcopy test failed.\n");
    return 0;
  }

  printf("Putting PicoRV back into reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 0);

  printf("Load PicoRV ByteCopy Program...\n");

  picorv_load_program(picorv_bytecopy_single_picobin, picorv_bytecopy_single_picobin_len);

  printf("Taking PicoRV out of reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

  printf("Internal memory bytecopy test\n");

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<36; ii++) {
    srcBufBytes[ii] = (unsigned char)rand();
    dstBufBytes[ii] = 0;
  }

  srcPtrBytes = srcBufBytes + 1;
  dstPtrBytes = dstBufBytes + 3;

  dma_copy(srcPtrBytes, dstPtrBytes, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrBytes, dstPtrBytes, 32)) {
    printf("PicoRV Bytecopy test successful.\n");
  }
  else {
    printf("PicoRV Bytecopy test failed.\n");
    return 0;
  }

  printf("External memory bytecopy test, port 0.\n");
  srcPtrBytes = (unsigned char*)MAIN_RAM_BASE+1;
  dstPtrBytes = (unsigned char*)(MAIN_RAM_BASE + 1024 + 3);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrBytes[ii] = (unsigned char)rand();
    dstPtrBytes[ii] = 0;
  }

  dma_copy(srcPtrBytes, dstPtrBytes, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrBytes, dstPtrBytes, 32)) {
    printf("PicoRV Bytecopy test successful.\n");
  }
  else {
    printf("PicoRV Bytecopy test failed.\n");
    return 0;
  }

  printf("External memory bytecopy test, port 1.\n");
  srcPtrBytes = (unsigned char*)MAIN_RAM_BASE2+1;
  dstPtrBytes = (unsigned char*)(MAIN_RAM_BASE2 + 1024 + 3);

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrBytes[ii] = (unsigned char)rand();
    dstPtrBytes[ii] = 0;
  }

  dma_copy(srcPtrBytes, dstPtrBytes, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrBytes, dstPtrBytes, 32)) {
    printf("PicoRV Bytecopy test successful.\n");
  }
  else {
    printf("PicoRV Bytecopy test failed.\n");
    return 0;
  }

  printf("External memory bytecopy to VRAM test\n");
  srcPtrBytes = (unsigned char*)MAIN_RAM_BASE+1; //+1 to create an unaligned address.
  dstPtrBytes = (unsigned char*)(VERA_VRAM_BASE + 3); //+3 to create another unaligned address.

  //Fill source buffer with some random data and destination with 0s
  for (int ii=0; ii<32; ii++) {
    srcPtrBytes[ii] = (unsigned char)rand();
    dstPtrBytes[ii] = 0;
  }

  dma_copy(srcPtrBytes, dstPtrBytes, 32);

  printf("Checking result...\n");
  if (!memcmp(srcPtrBytes, dstPtrBytes, 32)) {
    printf("PicoRV Bytecopy test successful.\n");
  }
  else {
    printf("PicoRV Bytecopy test failed.\n");
    return 0;
  }

  printf("PicoRV DMA tests successful.\n");

  return 0;
}
