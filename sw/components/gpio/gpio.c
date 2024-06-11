
#include "gpio.h"
#include <stdint.h>

void gpio_init(struct gpio * module, volatile void * base_address)
{
  module->registers = base_address;
  //Enable gpio global IRQ
  module->registers[GPIO_RGPIO_CTRL] = GPIO_RGPIO_CTRL_INTE_MSK;
}

void gpio_set_direction(struct gpio * module, uint32_t dir)
{
  module->registers[GPIO_RGPIO_OE] = dir;
}

uint32_t gpio_get_input(struct gpio * module)
{
  return module->registers[GPIO_RGPIO_IN];
}

void gpio_set_output(struct gpio * module, uint32_t output)
{
  module->registers[GPIO_RGPIO_OUT] = output;
}

void gpio_set_pin(struct gpio * module, uint8_t pin)
{
  module->registers[GPIO_RGPIO_OUT] |= (1 << pin);
}

void gpio_clear_pin(struct gpio * module, uint8_t pin)
{
  module->registers[GPIO_RGPIO_OUT] &= ~(1 << pin);
}

void gpio_irq_enable(struct gpio * module, uint32_t enable_mask, uint32_t edge_mask) {
  module->registers[GPIO_RGPIO_INTE] = enable_mask;
  module->registers[GPIO_RGPIO_PTRIG] = edge_mask;
}

void gpio_irq_ack(struct gpio * module, uint32_t ack_mask) {
  module->registers[GPIO_RGPIO_INTS] = ~ack_mask;
}

void gpio_enable_latch_clk(struct gpio * module, uint32_t enable_mask) {
  module->registers[GPIO_RGPIO_ECLK] = enable_mask;
}

