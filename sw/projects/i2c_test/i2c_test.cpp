#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "i2c.h"
#include "sdram.h"
#include "interrupts.h"
#include "cli.h"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.
#define TEST_STRING_SIZE 16

#define I2C_SLAVE_ADDR 0x6F
#define I2C_SLAVE_SRAM_START_ADDR 0x20

#define I2C_SPEED_SIM_HZ (PLATFORM_CLK_FREQ/40)
#define I2C_SPEED_FPGA_HZ 100000

static struct uart uart0;
static struct gpio gpio;

volatile int i2c_irqs_fired = 0;

extern "C" {
  //Just count and acknowledge interrupts
  void _i2c_irq_handler(void) {
    i2c.ackIRQ();
    ++i2c_irqs_fired;
  }

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
}

int test(void) {
  printf("Enabling IRQs\n");
  enable_global_irq();
  enable_irq(IRQ_ID_I2C);

  i2c.begin();
  i2c.enableIRQ(true);

  //GPIO bits 7:4 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio) & 0xf0) == GPIO_SIM_INDICATOR) {
    printf("This is a simulation.\n");

    i2c.setClock(I2C_SPEED_SIM_HZ);
  }
  else {
    printf("This is not a simulation.\n");

    i2c.setClock(I2C_SPEED_FPGA_HZ);
  }

  uint8_t write_string[TEST_STRING_SIZE] = "I2C test string";

  printf("Sending test string to I2C slave.\n");

  i2c.beginTransmission(I2C_SLAVE_ADDR);
  i2c.write(I2C_SLAVE_SRAM_START_ADDR);
  for (int ii=0; ii<TEST_STRING_SIZE; ii++) {
    i2c.write(write_string[ii]);
  }
  uint8_t res = i2c.endTransmission();
  assert(res == 0);

  printf("Reading back from slave.\n");
  //Read back from slave
  char readback_string[TEST_STRING_SIZE] = "               ";

  i2c.beginTransmission(I2C_SLAVE_ADDR);
  i2c.write(I2C_SLAVE_SRAM_START_ADDR);
  res = i2c.endTransmission();
  assert(res==0);

  res = i2c.requestFrom(I2C_SLAVE_ADDR, TEST_STRING_SIZE);
  assert(res == 0);

  for (int ii=0; ii<TEST_STRING_SIZE; ii++) {
    readback_string[ii] = i2c.read();
  }

  printf("Comparing strings.\n");
  //Compare strings
  if (memcmp(readback_string, write_string, TEST_STRING_SIZE)) {
    printf("I2C write-readback failed. Strings don't match.\n");

    for (int ii=0; ii<TEST_STRING_SIZE; ii++) {
      printf("Sent: 0x%x Received: 0x%x\n", write_string[ii], readback_string[ii]);
    }

    return -1;
  }
  else {
    printf("OK.\n");
  }

  printf("I2C IRQS received: %d\n", i2c_irqs_fired);
  if (i2c_irqs_fired != 2) {
    printf("Expected 2 I2C IRQS.\n");
    return -2;
  }

  return 0;
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  printf("\n\n");

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  if (!test()) {
    printf("Test Successful.\n");
  }
  else {
    printf("Test failed.\n");
  }

  printf("Push btn[0] to start CLI.\n");

  while ((gpio_get_input(&gpio) & 0x0100) == 0);

  cli(&uart0);
}

