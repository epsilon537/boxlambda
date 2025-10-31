#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdram.h"
#include "memmap.h"
#include "flashdrvr.h"
#include "version.h"
#include "crt0.h"

#define GPIO_PRE_SDRAM 0x1
#define GPIO_SDRAM_OK 0x2
#define GPIO_SDRAM_FAIL 0x3
#define GPIO_SIM_INDICATOR 0xf0
#define GPIO_SPIN_INDICATOR 0x0
#define GPIO_PRE_FLASH_COPY 0x4
#define GPIO_POST_FLASH_COPY 0x5

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(int, char **) {

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  gpio_set_output(GPIO_PRE_SDRAM);

  //On FPGA, wait 1s before outputting first message to make sure the terminal emulator
  //can catch up when plugging in the device.
  if ((gpio_get_input() & 0xf0) != GPIO_SIM_INDICATOR)
    usleep(1000000);

  printf("BoxLambda bootloader\n");
  printf("--------------------\n");
  printf("Version: %s\n", VERSION_STR);

  printf("Initializing SDRAM...\n");
  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    gpio_set_output(GPIO_SDRAM_OK);
    printf("Done.\n");
  }
  else {
    gpio_set_output(GPIO_SDRAM_FAIL);
    printf("SDRAM init failed!\n");
    while(1);
  }

  printf("Done.\n");

  gpio_set_output(GPIO_PRE_FLASH_COPY);

  uint32_t app_header_magic_num = *(uint32_t *)SPIFLASH_SW_BASE;
  if (app_header_magic_num == IMAGE_HEADER_MAGIC_NUMBER) {
    printf("Application image magic number check OK.\n");
  }
  else {
    printf("Application image magic number check failed. Expected 0x%x as first word, but found 0x%x\n", IMAGE_HEADER_MAGIC_NUMBER, app_header_magic_num);
    while(1);
  }

  uint32_t app_image_len = ((uint32_t *)SPIFLASH_SW_BASE)[1];

  printf("Application image size: %d bytes\n", app_image_len);

  printf("Copying SW image from Flash to DDR...\n");
  memcpy((void*)SDRAM_BASE, (void*)SPIFLASH_SW_BASE, app_image_len);
  printf("Done.\n");

  gpio_set_output(GPIO_POST_FLASH_COPY);
  //Check switches to see if we should spin or proceed.
  if ((gpio_get_input() & 0xf0) == GPIO_SPIN_INDICATOR) {
    printf("Spinning while switches are all 0.\n");
    while ((gpio_get_input() & 0xf0) == GPIO_SPIN_INDICATOR);
  }

  printf("Booting application image...\n");

  uart_tx_flush();

  __asm__ volatile (
    "la     t0, %[tgt]     \n" // destination address (constant)
    "jr     0x8(t0)        \n"
    :
    : [tgt] "i" (SDRAM_BASE)
    : "t0"
  );
}

