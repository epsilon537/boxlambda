#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "timer.h"
#include "interrupts.h"

static struct uart uart0;
static struct gpio gpio;

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);

  mcycle_start();
  disable_all_irqs();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

volatile int timer_irq_fired = 0;
volatile int uart_rx_irq_fired = 0;
volatile int uart_rx_fifo_irq_fired = 0;
volatile int uart_tx_fifo_empty_fired = 0;
volatile int uart_tx_fifo_half_empty_fired = 0;
volatile int gpio_irq_fired = 0;

void _gpio_irq_handler(void) {
  uint32_t isr = gpio_get_irq_status(&gpio);

  //Clear interrupt(s)
  gpio_irq_ack(&gpio, isr);

  gpio_irq_fired = 1;

  //Check pins 8-11. Drive pins 0-3.
  for (uint8_t ii=0; ii<4 ; ii++) {
    uint8_t in_pin = ii+8;
    uint8_t out_pin = ii;

    if (isr & (1<<in_pin)) {
      int pin_ptrig = gpio_get_ptrig_pin(&gpio, in_pin);

      //Now trigger on opposite edge
      gpio_set_ptrig_pin(&gpio, in_pin, pin_ptrig ? 0 : 1);

      //Make output (led) track input
      gpio_set_pin_value(&gpio, out_pin, pin_ptrig);
    }
  }

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

void _uart_irq_handler(void) {
  unsigned ien = uart_get_ien(&uart0);
  unsigned isr = uart_get_isr(&uart0);

  if (isr & ien & UART_IRQ_RX_DATA_AVL_MASK) {
    uart_irq_ack(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
    uart_rx_irq_fired = 1;
  }

  if (isr & ien & UART_IRQ_RX_FIFO_HALF_FULL_MASK) {
    uart_irq_ack(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);
    uart_rx_fifo_irq_fired = 1;
  }

  if (isr & ien & UART_IRQ_TX_FIFO_EMPTY_MASK) {
    uart_irq_ack(&uart0, UART_IRQ_TX_FIFO_EMPTY_MASK);
    uart_tx_fifo_empty_fired = 1;
  }

  if (isr & ien & UART_IRQ_TX_FIFO_HALF_EMPTY_MASK) {
    uart_irq_ack(&uart0, UART_IRQ_TX_FIFO_HALF_EMPTY_MASK);
    uart_tx_fifo_half_empty_fired = 1;
  }

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

void _timer_irq_handler(void) {
  //Stop the timer. If we don't stop it, or move into the future, the IRQ will keep on firing.
  mtimer_disable_raw_time_cmp();
  timer_irq_fired = 1;

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

int main(void) {
  //Switches
  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 outputs, 20 inputs

  printf("Enabling Ibex IRQs\n");
  enable_global_irq();
  enable_irq(IRQ_ID_TIMER);
  enable_irq(IRQ_ID_UART);
  enable_irq(IRQ_ID_GPIO);

  //----------------------------------------------
  printf("Setting timer...\n");

  timer_irq_fired = 0;
  mtimer_set_raw_time_cmp(10000); //Fire IRQ in 10000 ticks.
  while (timer_irq_fired == 0); //Wait for it.
  printf("Timer irq fired.\n");

  timer_irq_fired = 0;

  //Let's do it again.
  mtimer_set_raw_time_cmp(10000);
  while (timer_irq_fired == 0);
  printf("Timer irq fired.\n");
  disable_irq(IRQ_ID_TIMER);

  printf("Timer Test Successful.\n");

  //----------------------------------------------

  printf("Testing UART TX IRQs...\n");

  //This is enough to fill the UART TX FIFO more than halfway.
  uart_tx_string(&uart0, "0123456789\n");

  //Clear any pending Tx interrupts before enabling them in the UART core.
  uart_irq_ack(&uart0, UART_IRQ_TX_FIFO_EMPTY_MASK);
  uart_irq_ack(&uart0, UART_IRQ_TX_FIFO_HALF_EMPTY_MASK);
  uart_irq_en(&uart0, UART_IRQ_TX_FIFO_EMPTY_MASK);
  uart_irq_en(&uart0, UART_IRQ_TX_FIFO_HALF_EMPTY_MASK);

  while (!uart_tx_fifo_half_empty_fired); //Wait until half empty
  //At this point we shouldn't be completely empty yet.
  assert(!uart_tx_fifo_empty_fired);
  while (!uart_tx_fifo_empty_fired); //Wait until completely empty.
  //Disable UART TX IRQs again.
  uart_irq_dis(&uart0, UART_IRQ_TX_FIFO_EMPTY_MASK);
  uart_irq_dis(&uart0, UART_IRQ_TX_FIFO_HALF_EMPTY_MASK);

  printf("UART TX IRQ test successful.\n");

  //----------------------------------------------

  printf("Testing UART RX IRQs...\n");

  //Clear any pending IRQs in the UART core, then enable receive IRQs.
  uart_irq_ack(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
  uart_irq_ack(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);
  uart_irq_en(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
  uart_irq_en(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);

  int uart_rx_irq_count = 0;
  int uart_rx_fifo_irq_count = 0;
  int done = 0;
  char rxc = 0; //Receive character.

  printf("Please enter a character.\n");

  while (!done) {
    //The test bench will insert some characters into the UART.
    if (uart_rx_irq_fired) {
      printf("UART RX IRQ received.\n");
      uart_rx_irq_fired = 0;

      rxc = getc(stdin); //Pull the received character out of the FIFO.
      printf("Received character: %c\n", rxc);

      ++uart_rx_irq_count;
      if (uart_rx_irq_count == 2) {
        uart_irq_dis(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
        done = 1;
      }
      else {
        printf("Please enter another character.\n");
      }
    }
  }

  done = 0;

  printf("Please enter 8 characters. They will be echoed when all 8 characters are received.\n");

  while (!done) {
    //Fires when the RX FIFO is half full.
    if (uart_rx_fifo_irq_fired ) {
      printf("UART RX FIFO IRQ received.\n");
      uart_rx_fifo_irq_fired = 0;
      ++uart_rx_fifo_irq_count;
      if (uart_rx_fifo_irq_count == 2) {
        uart_irq_dis(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);
      }

      //Pull the received characters out of the FIFO.
      while (uart_rx_ready(&uart0)) {
        rxc = uart_rx(&uart0);
        printf("Received character: %c\n", rxc);
      }

      //When the interrupt is received twice, this test phase is done.
      if (uart_rx_fifo_irq_count == 2) {
        printf("UART RX IRQ Test Successful.\n");
        done = 1;
      }
      else {
        printf("Please enter 8 characters again. They will be echoed when all 8 characters are received.\n");
      }
    }
  }

  //----------------------------------------------

  printf("Testing GPIO IRQs...\n");

  //Enable GPIO IRQs on pins 8-11 (buttons) positive edge
  gpio_irq_enable(&gpio, 0xf00, 0xf00);

  printf("Push some buttons. The LEDS should track the button presses/releases.\n");

  while (1) {
    if (gpio_irq_fired) {
      printf("GPIO IRQ received.\n");
      gpio_irq_fired = 0;
    }
  }

  return 0;
}

