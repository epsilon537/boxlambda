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

#define BOOTLOADER_VERSION_MAJOR 0
#define BOOTLOADER_VERSION_MINOR 1

#define GPIO_PRE_SDRAM 0x1
#define GPIO_SDRAM_OK 0x2
#define GPIO_SDRAM_FAIL 0x3
#define GPIO_SIM_INDICATOR 0xf0

#ifdef __cplusplus
extern "C" {
#endif
//The second stage bootloader image is linked in as a binary image.
extern unsigned char _binary_second_stage_boot_bin_start[];
extern unsigned char _binary_second_stage_boot_bin_end[];
#ifdef __cplusplus
}
#endif

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  gpio_set_output(GPIO_PRE_SDRAM);

  //Wait 1s before outputting first message to make sure the terminal emulator
  //can catch up when plugging in the device.
  if ((gpio_get_input() & 0xf0) == GPIO_SIM_INDICATOR)
  //usleep(1000000);

  printf("BoxLambda first stage bootloader V%d.%d.\n", BOOTLOADER_VERSION_MAJOR, BOOTLOADER_VERSION_MINOR);
  printf("--------------------------------------\n");

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

  printf("Installing second stage bootloader in DDR...\n");

  memcpy((void*)SDRAM_BASE,
         _binary_second_stage_boot_bin_start,
         _binary_second_stage_boot_bin_end - _binary_second_stage_boot_bin_start);

  printf("Done.\n");
  printf("Proceeding to boot stage 2...\n");

  __asm__ volatile (
    "la     t0, %[tgt]     \n" // destination address (constant)
    "jr     0x80(t0)       \n"
    :
    : [tgt] "i" (SDRAM_BASE)
    : "t0"
  );
}

