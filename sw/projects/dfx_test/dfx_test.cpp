/* DFX test program to run on the dfx_test gateware build. The program presents */
/* a CLI through which the user can live-load reconfigurable modules such as the */
/* vs0_j1b core into the VS0 'black box' slot/partition of the BoxLambda SoC. */

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
#include "embedded_cli_setup.h"
#include "peek_poke_cli.h"
#include "dfx_cli.h"
#include "ymodem_cli.h"
#include "mem_fs_cli.h"
#include "j1b_cli.h"
#include "ff.h"

#define STR_ROOT_DIRECTORY ""

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

  printf("Mounting filesystem...\n");

  static FATFS fs;

  /* Clear file system object */
  memset(&fs, 0, sizeof(FATFS));

  FRESULT res = f_mount(&fs, "", 1);
  if (res != FR_OK) {
    printf("FatFS mount error! %d\n", res);
    return -1;
  }

  printf("Starting CLI...\n");

  EmbeddedCli *cli = createEmbeddedCli(&uart0);

  //CLI command for peeking an poking memory/registers
  add_peek_poke_cli(cli);
  //DFX - Dynamic Function Exchange CLI commands through which the user can live-load the bitstream of a
  //gateware component into the VS0 black box slot/partition of the BoxLambda SoC.
  add_dfx_cli(cli);
  //CLI commands to interact with the filesystem.
  add_mem_fs_cli(cli);
  //CLI commands to transfer files via the ymodem serial protocol.
  add_ymodem_cli(cli, &uart0);
  //CLI commands to interact with the J1B core after it's been loaded into the system using the DFX CLI.
  add_j1b_cli(cli, &uart0);

  embeddedCliStartLoop();

  return 0;
}

