#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "timer.h"
#include "interrupts.h"
#include "picorv_dma_hal.h"
#include "picorv_irq_in_out.h"

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;


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
}

void _timer_irq_handler(void) {
  //Stop the timer. If we don't stop it, or move into the future, the IRQ will keep on firing.
  mtimer_disable_raw_time_cmp();
  timer_irq_fired = 1;
}

int main(void) {
  //Switches
  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  //Buttons
  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  printf("Load PicoRV Program picorv_irq_in_out.\n");
  //This test program just ors any recevied interrupts into the irq_out
  //system register.
  picorv_load_program(picorv_irq_in_out_picobin, picorv_irq_in_out_picobin_len);

  printf("Taking PicoRV out of reset...\n");
  picorv_sys_reg_wr(PICORV_SYS_REG_CTRL, 1);

  printf("Enabling IRQs\n");
  enable_global_irq();
  enable_irq(IRQ_ID_TIMER);
  enable_irq(IRQ_ID_UART);

  //Clear any pending IRQs in the UART core, then enable received IRQs.
  uart_irq_ack(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
  uart_irq_ack(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);
  uart_irq_en(&uart0, UART_IRQ_RX_DATA_AVL_MASK);
  uart_irq_en(&uart0, UART_IRQ_RX_FIFO_HALF_FULL_MASK);

  printf("Setting timer...\n");

  timer_irq_fired = 0;
  mtimer_set_raw_time_cmp(10000); //Fire IRQ in 10000 ticks.
  while (timer_irq_fired == 0); //Wait for it.
  printf("Timer irq fired.\n");
  //Check if the PicoRV DMA also saw the interrupt.
  assert(picorv_sys_reg_rd(PICORV_SYS_REG_IRQ_OUT) & (1<<IRQ_ID_TIMER));
  //Acknowledge the interrupt in the picorv dma core.
  picorv_sys_reg_wr(PICORV_SYS_REG_IRQ_OUT, 1<<IRQ_ID_TIMER);

  timer_irq_fired = 0;

  //Let's do it again.
  mtimer_set_raw_time_cmp(10000);
  while (timer_irq_fired == 0);
  printf("Timer irq fired.\n");
  disable_irq(IRQ_ID_TIMER);
  assert(picorv_sys_reg_rd(PICORV_SYS_REG_IRQ_OUT) & (1<<IRQ_ID_TIMER));
  picorv_sys_reg_wr(PICORV_SYS_REG_IRQ_OUT, 1<<IRQ_ID_TIMER);

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
  //Check if the PicoRV DMA core saw the interrupt as well.
  assert(picorv_sys_reg_rd(PICORV_SYS_REG_IRQ_OUT) & (1<<IRQ_ID_UART));
  picorv_sys_reg_wr(PICORV_SYS_REG_IRQ_OUT, 1<<IRQ_ID_UART);
  while (!uart_tx_fifo_empty_fired); //Wait until completely empty.

  uart_irq_dis(&uart0, UART_IRQ_TX_FIFO_EMPTY_MASK);
  uart_irq_dis(&uart0, UART_IRQ_TX_FIFO_HALF_EMPTY_MASK);

  printf("UART TX IRQs received.\n");

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

      assert(picorv_sys_reg_rd(PICORV_SYS_REG_IRQ_OUT) & (1<<IRQ_ID_UART));
      picorv_sys_reg_wr(PICORV_SYS_REG_IRQ_OUT, 1<<IRQ_ID_UART);

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

  printf("Please enter 8 characters. They will be echoed when all 8 characters are received.\n");

  done = 0;

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
        done = 1;
      }
      else {
        printf("Please enter 8 characters again. They will be echoed when all 8 characters are received.\n");
      }
    }
  }

  disable_global_irq();

  printf("Test Successful.\n");
  return 0;
}

