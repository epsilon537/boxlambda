#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c_master_hal.h"
#include "interrupts.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.
#define TEST_STRING_SIZE 16

#define I2C_SLAVE_ADDR 0x6F
#define I2C_SLAVE_SRAM_START_ADDR 0x20

#define I2C_SPEED_SIM 40
#define I2C_SPEED_FPGA 500

static struct uart uart0;
static struct gpio gpio;

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
  uint32_t leds = 0xF;

  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  //GPIO bits 7:4 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio) & 0xf0) == GPIO_SIM_INDICATOR) {
    printf("This is a simulation.\n");
    //Set bus speed - divide 50MHz system clock by 40
    i2c_master_set_speed(I2C_SPEED_SIM);

    assert(i2c_master_get_speed() == I2C_SPEED_SIM);
  }
  else {
    printf("This is not a simulation.\n");

    //Set bus speed - divide 50MHz system clock by 500 -> 100kHz
    i2c_master_set_speed(I2C_SPEED_FPGA);

    assert(i2c_master_get_speed() == I2C_SPEED_FPGA);
  }

  char write_string[TEST_STRING_SIZE] = "I2C test string";

  printf("Sending test string.\n");
  //Write to slave SRAM, starting address 7
  i2c_master_write_bytes(write_string, I2C_SLAVE_ADDR, I2C_SLAVE_SRAM_START_ADDR, TEST_STRING_SIZE);

  printf("Waiting for idle...\n");
  while (i2c_master_busy());

  assert(!i2c_master_err());

  volatile char *buf_ptr = i2c_master_get_buf_ptr();

  //Compare I2C master buffer contents with test string
  if (memcmp((void*)(buf_ptr+I2C_SLAVE_SRAM_START_ADDR), write_string, TEST_STRING_SIZE)) {
    printf("I2C master buffer contents incorrect.\n");
    for (int ii=0; ii<TEST_STRING_SIZE; ii++) {
      printf("Expected: 0x%x, Got: 0x%x\n", write_string[ii], buf_ptr[ii+I2C_SLAVE_SRAM_START_ADDR]);
    }

    return 1;
  }
  else {
    printf("I2C master buffer contents correct.\n");
  }

  printf("Clearing I2C master buffer.\n");
  //Clear I2C master buffer
  memset((void*)buf_ptr, 0, I2C_MASTER_BUF_SIZE_BYTES);

  printf("Reading back from slave.\n");
  //Read back from slave
  char readback_string[TEST_STRING_SIZE] = "               ";
  i2c_master_read_bytes(readback_string, I2C_SLAVE_ADDR, I2C_SLAVE_SRAM_START_ADDR, TEST_STRING_SIZE);

  printf("Comparing strings.\n");
  //Compare strings
  if (memcmp(readback_string, write_string, TEST_STRING_SIZE)) {
    printf("I2C write-readback failed. Strings don't match.\n");

    for (int ii=0; ii<TEST_STRING_SIZE; ii++) {
      printf("Sent: 0x%x Received: 0x%x\n", write_string[ii], readback_string[ii]);
    }
  }
  else {
    printf("Test Successful.\n");
  }

  for (;;) {
    gpio_set_output(&gpio, leds);
    leds ^= 0xF;

    if ((gpio_get_input(&gpio) & 0xf0) == GPIO_SIM_INDICATOR)
      usleep(500 * 10); //Sleep less when we're running inside a simulator.
    else
      usleep(500 * 1000);
  }
}

