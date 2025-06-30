// This test program checks if SW can retrieve the reset reason
// from the reset controller and if SW can trigger a reset.

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdram.h"
#include "reset_regs.h"

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);

  mcycle_start();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void  _exit (int status) {
  while (1);
}

void print_reset_reason() {
  //Read the reset reason
  uint32_t reset_reason = RESET->REASON;

  if (reset_reason & RESET_REASON_POR_MASK ) {
    printf("Reset Reason: Power-On Reset.\n");
  }

  if (reset_reason & RESET_REASON_SW_NDM_MASK ) {
    printf("Reset Reason: SW triggered Non-Debug Module Reset.\n");
  }

  if (reset_reason & RESET_REASON_SW_DM_MASK ) {
    printf("Reset Reason: SW triggered Debug Module Reset.\n");
  }

  if (reset_reason & RESET_REASON_NDM_MASK ) {
    printf("Reset Reason: Non-Debug Module Reset.\n");
  }

  if (reset_reason & RESET_REASON_EXT_MASK ) {
    printf("Reset Reason: External Reset.\n");
  }

  if (reset_reason & RESET_REASON_SW_USB_MASK ) {
    printf("Reset Reason: SW triggered USB Reset.\n");
  }

  if (reset_reason == 0) {
    printf("Reset Reason: Unknown.\n");
  }
}

int main(void) {
  uint32_t leds = 0xF;

  printf("Starting...\n");

  //Switches and LEDs
  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  //We need SDRAM in this build because the flashdriver requires
  //heap memory, which is located in SDRAM.

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  print_reset_reason();

  printf("Push btn[0] to SW trigger DM+NDM Reset.\n");

  for (;;) {
    /*SW trigger NDM reset if btn 0 is pushed.*/
    if ((gpio_get_input() & 0x0100) != 0) {
      printf("SW triggering DM+NDM reset...\n");
      RESET->CTRL = RESET_CTRL_DM_RESET_MASK|RESET_CTRL_NDM_RESET_MASK;
    }
  }

  return 0;
}

