// This test program boots the J1B core and forwards the UART to this core
// so the user can interact with J1B's Forth REPL.

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "vs0_hal.h" //To read the core signature register.
#include "j1b_hal.h"
#include "j1b_nuc.h"

/* This is the Forth run-time firmware image to be loaded into the J1B core
 * before taking it out of reset. */
static unsigned j1b_nuc_prg[] = J1B_NUC_PRG;

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio;

#ifdef __cplusplus
extern "C"
{
#endif
//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);

  mcycle_start();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void  _exit (int status) {
  while (1);
}
#ifdef __cplusplus
}
#endif

int main(void) {
  uint32_t leds = 0xF;

  printf("Starting...\n");

  //Switches and LEDs
  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  printf("Reading J1B core signature register...\n");

  uint32_t sig = vs0_reg_rd(VS0_REG_SIGNATURE);

  if (sig != J1B_SIG_VALUE) {
    printf("Incorrect signature! Received 0x%x, Expected 0x%x. Aborting...\n", sig, J1B_SIG_VALUE);
    return -1;
  }

  printf("Signature correct.\n");

  printf("J1B program length in bytes: %d\n", sizeof(j1b_nuc_prg));

  j1b_load_program((unsigned char*)j1b_nuc_prg, sizeof(j1b_nuc_prg));

  printf("Taking J1B out of reset...\n");

  j1b_reg_wr(J1B_REG_CTRL, J1B_REG_CTRL_RST_N);

  unsigned socUartRx;
  unsigned j1bUartRx;

  char testString[] = "42 EMIT\n";

  printf("Sending test string: %s\n", testString);

  for (int ii=0; ii<sizeof(testString); ii++) {
    while(j1b_reg_rd(J1B_REG_UART_TX_TO_J1B) & J1B_REG_UART_TX_TO_J1B_DATA_WAITING);

    j1b_reg_wr(J1B_REG_UART_TX_TO_J1B, testString[ii]);

    j1bUartRx = j1b_reg_rd(J1B_REG_UART_RX_FROM_J1B);
    if (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_DATA_AVL) {
       uart_tx(&uart0, (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_RX_DATA_MSK));
    }
  }

  printf("Test string sent. Forwarding UART...\n");

  for (;;) {
    if (uart_rx_ready(&uart0)) {
      socUartRx = (char)uart_rx(&uart0);

      while(j1b_reg_rd(J1B_REG_UART_TX_TO_J1B) & J1B_REG_UART_TX_TO_J1B_DATA_WAITING);

      j1b_reg_wr(J1B_REG_UART_TX_TO_J1B, socUartRx);
    }

    j1bUartRx = j1b_reg_rd(J1B_REG_UART_RX_FROM_J1B);
    if (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_DATA_AVL) {
       while(!uart_tx_ready(&uart0));
       uart_tx(&uart0, (j1bUartRx&J1B_REG_UART_RX_FROM_J1B_RX_DATA_MSK));
    }
  }

  return 0;
}

