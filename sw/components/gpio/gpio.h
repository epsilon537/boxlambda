
#ifndef GPIO_H
#define GPIO_H

#include <stdint.h>
#include "gpio_regs.h"

#ifdef __cplusplus
extern "C" {
#endif

struct gpio
{
	volatile uint32_t * registers;
};

/*
 * Initializes a GPIO instance.
 * @param module		Pointer to a GPIO instance structure.
 * @param base_address	Pointer to the base address of the GPIO hardware instance.
 */
void gpio_init(struct gpio * module, volatile void * base_address);

/*
 * Sets the GPIO direction register.
 * 1 for output, 0 for input
 * @param module Pointer to a GPIO instance structure.
 * @param dir    Direction bitmask for the GPIO direction register.
 */
void gpio_set_direction(struct gpio * module, uint32_t dir);

uint32_t gpio_get_input(struct gpio * module);

void gpio_set_output(struct gpio * module, uint32_t output);

/*
 * Sets (turns on) the specified GPIO pin.
 * @param module Pointer to the GPIO instance structure.
 * @param pin    Pin number for the pin to turn on.
 */
void gpio_set_pin(struct gpio * module, uint8_t pin);

/*
 * Clears (turns off) the specified GPIO pin.
 * @param module Pointer to the PGIO instance structure.
 * @param pin    Pin number for the pin to turn off.
 */
void gpio_clear_pin(struct gpio * module, uint8_t pin);

/*
 * Enable/disable IRQs for given pins.
 * @param module Pointer to the GPIO instance structure.
 * @param enable_mask bit mask of GPIO pins for which to enable IRQs.
 * @param edge_mask bit mask specifying for each pin on which edge to trigger IRQ: 0=falling edge, 1=rising edge.
 */
void gpio_irq_enable(struct gpio * module, uint32_t enable_mask, uint32_t edge_mask);

/*
 * Acknowledge IRQs for given pins.
 * @param module Pointer to the GPIO instance structure.
 * @param ack_mask bit mask of GPIO pins for which to acknowledge IRQs.
 */
void gpio_irq_ack(struct gpio * module, uint32_t ack_mask);

/*
 * Enable/disable gp_clk clock latching for given pins.
 * @param module Pointer to the GPIO instance structure.
 * @param enable_mask bit mask of GPIO pins for which to enable gp_clk clock latching.
 */
void gpio_enable_latch_clk(struct gpio * module, uint32_t enable_mask);

#ifdef __cplusplus
}
#endif

#endif /* GPIO_H */

