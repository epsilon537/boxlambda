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
#include "sdspi_hal.h"
#include "wb.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

//Return 0 if OK, negative on error.
static int sdspi_test(void) {
	int res=0;
	unsigned v;
	unsigned	boot_sector[128], test_sector[128], buf[128];

	v = sdspi_read_aux();
	if (SDSPI_PRESENTN & wb_read(SDSPI_CMD_ADDR)) {
		printf("Waiting for the card assertion to be registered\n");
		while(SDSPI_PRESENTN & wb_read(SDSPI_CMD_ADDR))
			;
	}

	//
	// GO_IDLE
	printf("SEND_GO_IDLE\n");
	if (0x01 != sdspi_sdcmd(SDSPI_GO_IDLE,0)) {
		printf("Go idle failed.\n");
		return -1;
  	}

	//
	// SEND_IF_COND
	printf("SEND_IF_COND\n");
	if (0 != sdspi_sdcmd(SDSPI_READREG | SDSPI_CMD + 8, 0x01a5)) {
		printf("send_if_cond failed\n");
		return -1;
	}

  	printf("Data response = 0x%08x\n", wb_read(SDSPI_DATA_ADDR));
	if (0x01a5 != wb_read(SDSPI_DATA_ADDR)) {
		printf("send_if_cond data response failed.\n");
		return -1;
	}
	//
	// Wait for the card to start up
	do {
		if (0 != sdspi_sdcmd(SDSPI_ACMD,0) & 0x01) {
		printf("ACMD failed.\n");
		return -1;
		}

		if (0 != ((v = sdspi_sdcmd(SDSPI_CMD + 41, 0x40000000))&~1)) {
			printf("cmd+41 failed\n");
			return -1;
		}

	} while(v & 1);

	//
	// Read the OCR register
	sdspi_read_ocr();

	// Speed up our interface
	sdspi_set_aux(1);

	//
	// Read the CSD register
	sdspi_read_csd(test_sector);
  	printf("Read\n");
	printf("CSD: ");
	for(int k=0; k<4; k++)
		printf("%08x%c", test_sector[k], (k < 3) ? ':':'\n');

	//
	// Read the CID register
	sdspi_read_cid(test_sector);
	printf("CID: ");
	for(int k=0; k<4; k++)
		printf("%08x%c", test_sector[k], (k < 3) ? ':':'\n');
	//
	// Read the original boot sector
	sdspi_read(SDSPI_READ_SECTOR, 0, 512, boot_sector);
	//
	// Write random data to the boot sector
	for(unsigned k=0; k<128; k++)
		test_sector[k] = rand();
	sdspi_write(SDSPI_WRITE_SECTOR | SDSPI_FIFO_ID, 0, 512, test_sector);

	//
	// Read the random data back
	sdspi_read(SDSPI_READ_SECTOR, 0, 512, buf);
	//
	// Check that it was correctly written
	for(unsigned k=0; k<128; k++) {
    	printf("BUF[%3d] = 0x%08x, TST[%3d] = 0x%08x\n", k, buf[k], k, test_sector[k]);
	}
	for(unsigned k=0; k<128; k++) {
		printf("BUF[%d] = 0x%08x\n", k, buf[k]);
		printf("TST[%d] = 0x%08x\n", k, test_sector[k]);
		if (buf[k] != test_sector[k]) {
			printf("Boot sector r/w test failed.\n");
			return -1;
		}
	}
	//
	// Restore the boot sector
  	sdspi_write(SDSPI_WRITE_SECTOR, 0, 512, boot_sector);
	//
	// Read it back again
	sdspi_read(SDSPI_READ_SECTOR | SDSPI_FIFO_ID, 0, 512, buf);
	//
	// Check that it was properly stored
	for(unsigned k=0; k<128; k++)
		if (buf[k] != boot_sector[k]) {
			printf("boot sector restore failed.\n");
			return -1;
		}

	printf("SDSPI Test successful.\n");

  return 0;
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  printf("Starting sdspi_test...\n");
  sdspi_test();
}
