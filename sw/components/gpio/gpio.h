
#ifndef GPIO_H
#define GPIO_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif
/*
 * Initializes a GPIO instance.
 */
void gpio_init(void);

/*
 * Gets the GPIO direction register.
 * 1 for output, 0 for input
 */
uint32_t gpio_get_direction(void);

/*
 * Sets the GPIO direction register.
 * 1 for output, 0 for input
 * @param dir    Direction bitmask for the GPIO direction register.
 */
void gpio_set_direction(uint32_t dir);

/*
 * Retrieve GPIO input bit vector.
 */
uint32_t gpio_get_input(void);

/*
 * Set GPIO output bit vector.
 */
void gpio_set_output(uint32_t output);

/*
 * Set GPIO output value for a specific pin.
 * @param pin    Pin number
 * @param value  output value (must be 0 or 1)
 */
void gpio_set_pin_value(uint8_t pin, int value);

/*
 * Sets (turns on) the specified GPIO pin.
 * @param pin    Pin number for the pin to turn on.
 */
void gpio_set_pin(uint8_t pin);

/*
 * Clears (turns off) the specified GPIO pin.
 * @param pin    Pin number for the pin to turn off.
 */
void gpio_clear_pin(uint8_t pin);

/*
 * Enable/disable IRQs for given GPIO pins.
 * @param enable_mask bit mask of GPIO pins for which to enable IRQs.
 * @param edge_mask bit mask specifying for each pin on which edge to trigger IRQ: 0=falling edge, 1=rising edge.
 */
void gpio_irq_enable(uint32_t enable_mask, uint32_t edge_mask);

/*
 * Acknowledge IRQs for given pins.
 * @param ack_mask bit mask of GPIO pins for which to acknowledge IRQs. Bits set indicate which pin IRQs to clear.
 */
void gpio_irq_ack(uint32_t ack_mask);

/*
 * Enable/disable gp_clk clock latching for given pins.
 * @param enable_mask bit mask of GPIO pins for which to enable gp_clk clock latching.
 */
void gpio_enable_latch_clk(uint32_t enable_mask);

/*
 * Retrieve GPIO interrupt status bit vector. A bit set to 1 indicates that this pin has an
 * active interrupt.
 */
uint32_t gpio_get_irq_status(void);

/*
 * Retrieve GPIO PTRIG setting for a given pin. A return value of 1/0 indicates that this pin triggers
 * an interrupt (if enabled) on an positive/negative edge.
 * @param pin    Pin number
 */
uint32_t gpio_get_ptrig_pin(uint8_t pin);

/*
 * Set GPIO PTRIG value for a given pin. A bit set to 1/0 indicates that this pin triggers
 * an interrupt (if enabled) on an positive/negative edge.
 * @param pin    Pin number
 * @param value  PTRIG value (must be 0 or 1)
 */
void gpio_set_ptrig_pin(uint8_t pin, int value);

#ifdef __cplusplus
}
#endif

#endif /* GPIO_H */

