//This is stage 2 of the bootloader. It executes from DDR memory. All segments
//(code, heap, stack, bss, data...) are placed in DDR memory.

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "gpio.h"
#include "memmap.h"
#include "flashdrvr.h"
#include "uart.h"

#define GPIO_SPIN_INDICATOR 0x0

#define GPIO_PRE_FLASH_COPY 0x4
#define GPIO_POST_FLASH_COPY 0x5

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  printf("Bootloader stage 2:\n");
  printf("-------------------\n");

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  gpio_set_output(GPIO_PRE_FLASH_COPY);

  printf("Copying SW image from Flash to IMEM...\n");
  memcpy((void*)IMEM_BASE, (void*)SPIFLASH_SW_BASE, IMEM_SIZE_BYTES);
  printf("Done.\n");

  gpio_set_output(GPIO_POST_FLASH_COPY);

  //Check switches to see if we should spin or proceed.
  if ((gpio_get_input() & 0xf0) == GPIO_SPIN_INDICATOR) {
    printf("Spinning while switches are all 0.\n");
    while ((gpio_get_input() & 0xf0) == GPIO_SPIN_INDICATOR);
  }

  printf("Starting SW image...\n");

  uart_tx_flush();

 __asm__ volatile (
    //Jump to start vector
      "jr     0x80(x0)           \n"
  );
}

