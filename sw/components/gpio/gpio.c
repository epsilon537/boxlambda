
#include "gpio.h"
#include "gpio_regs.h"
#include <stdint.h>
#include <assert.h>

void gpio_init(void)
{
  //Enable gpio global IRQ
  GPIO->RGPIO_CTRL_STATUS_bf.INTE = 1;
}

uint32_t gpio_get_direction(void)
{
  return GPIO->RGPIO_OE;
}

void gpio_set_direction(uint32_t dir)
{
  GPIO->RGPIO_OE = dir;
}

uint32_t gpio_get_input(void)
{
  return GPIO->RGPIO_IN;
}

void gpio_set_output(uint32_t output)
{
  GPIO->RGPIO_OUT = output;
}

void gpio_set_pin_value(uint8_t pin, int value)
{
  uint32_t out = GPIO->RGPIO_OUT;
  assert((value == 1) || (value == 0));

  out &= ~(1 << pin);
  out |= (value << pin);

  GPIO->RGPIO_OUT = out;
}

void gpio_set_pin(uint8_t pin)
{
  GPIO->RGPIO_OUT = (1 << pin);
}

void gpio_clear_pin(uint8_t pin)
{
  GPIO->RGPIO_OUT = ~(1 << pin);
}

void gpio_irq_enable(uint32_t enable_mask, uint32_t edge_mask) {
  GPIO->RGPIO_INTE = enable_mask;
  GPIO->RGPIO_PTRIG = edge_mask;
}

void gpio_irq_ack(uint32_t ack_mask) {
  GPIO->RGPIO_INTS &= ~ack_mask;
}

void gpio_enable_latch_clk(uint32_t enable_mask) {
  GPIO->RGPIO_ECLK = enable_mask;
}

uint32_t gpio_get_irq_status()
{
  return GPIO->RGPIO_INTS;
}

void gpio_set_ptrig_pin(uint8_t pin, int value)
{
  uint32_t ptrig = GPIO->RGPIO_PTRIG;
  assert((value == 1) || (value == 0));

  ptrig &= ~(1 << pin);
  ptrig |= (value << pin);

  GPIO->RGPIO_PTRIG = ptrig;
}

uint32_t gpio_get_ptrig_pin(uint8_t pin)
{
  return (GPIO->RGPIO_PTRIG >> pin) & 1;
}

