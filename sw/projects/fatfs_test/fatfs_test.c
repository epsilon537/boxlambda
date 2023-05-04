#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>

#include "stdio_to_uart.h"
#include "uart.h"
//#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "ff.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

#define DISK_DEV_NUM       "0:"
#define STR_ROOT_DIRECTORY ""
const char *file_name = STR_ROOT_DIRECTORY "Basic.bin";
/** Size of the file to write/read.*/
#define DATA_SIZE 2048
static uint8_t data_buffer[DATA_SIZE];
#define TEST_SIZE   (4 * 1024)

static struct uart uart0;

static struct uart uart0;
//static struct gpio gpio0;
//static struct gpio gpio1;

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

//Returns 0 if OK. Negative on error
static int fatfs_test(void)
{
	uint32_t i;
	UINT byte_to_read;
	UINT byte_read;
	UINT byte_written;

	FRESULT res;
	DIR dirs;

	/* Declare these as static to avoid stack usage.
	 * They each contain an array of maximum sector size.
	 */
	static FATFS fs;
	static FIL file_object;

	/* Format disk */
	//res = f_mkfs("",
	//		0, 0,
	//		512); /* AllocSize */

	//if (res != FR_OK) {
	//	printf("FatFS make file system error! %d\n", res);
	//	return -1;
	//}

	/* Clear file system object */
	memset(&fs, 0, sizeof(FATFS));
	res = f_mount(&fs, "", 1);
	if (res != FR_OK) {
		printf("FatFS mount error! %d\n", res);
    	return -1;
	}

	/* Test if the disk is formated */
	res = f_opendir(&dirs, STR_ROOT_DIRECTORY);
	if (res != FR_OK) {
		printf("FatFS opendir error!\n");
    	return -1;
	}

	/* Create a new file */
	res = f_open(&file_object, (char const *)file_name,
			FA_CREATE_ALWAYS | FA_WRITE);
	if (res != FR_OK) {
		printf("FatFS file open error!\n");
    	return -1;
	}

	/* Write a checkerboard pattern in the buffer */
	for (i = 0; i < sizeof(data_buffer); i++) {
		if ((i & 1) == 0) {
			data_buffer[i] = (i & 0x55);
		} else {
			data_buffer[i] = (i & 0xAA);
		}
	}

	for (i = 0; i < TEST_SIZE; i += DATA_SIZE) {
		res = f_write(&file_object, data_buffer, DATA_SIZE,
				&byte_written);

		if (res != FR_OK) {
			printf("FatFS file write error!\n");
      		return -1;
		}
	}

	/* Close the file */
	res = f_close(&file_object);
	if (res != FR_OK) {
		printf("FatFS file close error!\n");
    	return -1;
	}

	/* Open the file */

	res = f_open(&file_object, (char const *)file_name,
			FA_OPEN_EXISTING | FA_READ);
	if (res != FR_OK) {
		printf("FatFS file open error!\n");
    	return -1;
	}

	/* Read file */
	memset(data_buffer, 0, DATA_SIZE);
	byte_to_read = f_size(&file_object);

	for (i = 0; i < byte_to_read; i += DATA_SIZE) {
		res = f_read(&file_object, data_buffer, DATA_SIZE, &byte_read);
		if (res != FR_OK) {
			printf("FatFS file read error!\n");
      		return -1;
		}
	}

	/* Close the file*/
	res = f_close(&file_object);
	if (res != FR_OK) {
		printf("FatFS file close error!\n");
    	return -1;
	}

	/* Compare read data with the expected data */
	for (i = 0; i < sizeof(data_buffer); i++) {
		if (!((((i & 1) == 0) && (data_buffer[i] == (i & 0x55))) ||
				(data_buffer[i] == (i & 0xAA)))) {
			printf("FatFS data compare error!\n");
      		return -1;
		}
	}

  printf("FatFS Test Completed Successfully!\n");
  return 0;
}

int main(void) {
  uint32_t leds = 0xF;

  //gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  //gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  //gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  //gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  printf("Starting fatfs_test...\n");
  fatfs_test();

  while(1);

  return 0;
}
