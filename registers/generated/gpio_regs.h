// Created with Corsair v1.0.4
#ifndef __GPIO_REGS_H
#define __GPIO_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define GPIO_BASE_ADDR 0x10000100

// RGPIO_IN - Latched value of general-purpose input pins.
#define GPIO_RGPIO_IN_ADDR 0x0
#define GPIO_RGPIO_IN_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // Latched value of general-purpose input pins.
  };
} gpio_rgpio_in_t;

// RGPIO_IN.PINS - Latched value of general-purpose input pins.
#define GPIO_RGPIO_IN_PINS_WIDTH 32
#define GPIO_RGPIO_IN_PINS_LSB 0
#define GPIO_RGPIO_IN_PINS_MASK 0xffffffff
#define GPIO_RGPIO_IN_PINS_RESET 0x0

// RGPIO_OUT - General-purpose output pin values.
#define GPIO_RGPIO_OUT_ADDR 0x4
#define GPIO_RGPIO_OUT_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // General-purpose output pin values.
  };
} gpio_rgpio_out_t;

// RGPIO_OUT.PINS - General-purpose output pin values.
#define GPIO_RGPIO_OUT_PINS_WIDTH 32
#define GPIO_RGPIO_OUT_PINS_LSB 0
#define GPIO_RGPIO_OUT_PINS_MASK 0xffffffff
#define GPIO_RGPIO_OUT_PINS_RESET 0x0

// RGPIO_OE - General-purpose pins output enables.
#define GPIO_RGPIO_OE_ADDR 0x8
#define GPIO_RGPIO_OE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // General-purpose pins output enables.
  };
} gpio_rgpio_oe_t;

// RGPIO_OE.PINS - General-purpose pins output enables.
#define GPIO_RGPIO_OE_PINS_WIDTH 32
#define GPIO_RGPIO_OE_PINS_LSB 0
#define GPIO_RGPIO_OE_PINS_MASK 0xffffffff
#define GPIO_RGPIO_OE_PINS_RESET 0x0

// RGPIO_INTE - General-purpose pin interrupt enables.
#define GPIO_RGPIO_INTE_ADDR 0xc
#define GPIO_RGPIO_INTE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // General-purpose pins interrupt enables.
  };
} gpio_rgpio_inte_t;

// RGPIO_INTE.PINS - General-purpose pins interrupt enables.
#define GPIO_RGPIO_INTE_PINS_WIDTH 32
#define GPIO_RGPIO_INTE_PINS_LSB 0
#define GPIO_RGPIO_INTE_PINS_MASK 0xffffffff
#define GPIO_RGPIO_INTE_PINS_RESET 0x0

// RGPIO_PTRIG - Trigger IRQ on positive edge.
#define GPIO_RGPIO_PTRIG_ADDR 0x10
#define GPIO_RGPIO_PTRIG_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // Trigger IRQ on positive edge if set, on negative edge if cleared.
  };
} gpio_rgpio_ptrig_t;

// RGPIO_PTRIG.PINS - Trigger IRQ on positive edge if set, on negative edge if cleared.
#define GPIO_RGPIO_PTRIG_PINS_WIDTH 32
#define GPIO_RGPIO_PTRIG_PINS_LSB 0
#define GPIO_RGPIO_PTRIG_PINS_MASK 0xffffffff
#define GPIO_RGPIO_PTRIG_PINS_RESET 0x0

// RGPIO_CTRL_STATUS - GPIO control and status register
#define GPIO_RGPIO_CTRL_STATUS_ADDR 0x18
#define GPIO_RGPIO_CTRL_STATUS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t INTE : 1; // Interrupt enabled
    uint32_t INTS : 1; // Interrupt status
    uint32_t : 30; // reserved
  };
} gpio_rgpio_ctrl_status_t;

// RGPIO_CTRL_STATUS.INTE - Interrupt enabled
#define GPIO_RGPIO_CTRL_STATUS_INTE_WIDTH 1
#define GPIO_RGPIO_CTRL_STATUS_INTE_LSB 0
#define GPIO_RGPIO_CTRL_STATUS_INTE_MASK 0x1
#define GPIO_RGPIO_CTRL_STATUS_INTE_RESET 0x0

// RGPIO_CTRL_STATUS.INTS - Interrupt status
#define GPIO_RGPIO_CTRL_STATUS_INTS_WIDTH 1
#define GPIO_RGPIO_CTRL_STATUS_INTS_LSB 1
#define GPIO_RGPIO_CTRL_STATUS_INTS_MASK 0x2
#define GPIO_RGPIO_CTRL_STATUS_INTS_RESET 0x0

// RGPIO_INTS - GPIO interrupt status register.
#define GPIO_RGPIO_INTS_ADDR 0x1c
#define GPIO_RGPIO_INTS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // Interrupt status - Write 0 to clear.
  };
} gpio_rgpio_ints_t;

// RGPIO_INTS.PINS - Interrupt status - Write 0 to clear.
#define GPIO_RGPIO_INTS_PINS_WIDTH 32
#define GPIO_RGPIO_INTS_PINS_LSB 0
#define GPIO_RGPIO_INTS_PINS_MASK 0xffffffff
#define GPIO_RGPIO_INTS_PINS_RESET 0x0

// RGPIO_ECLK - Latch on gp_clk input signal.
#define GPIO_RGPIO_ECLK_ADDR 0x20
#define GPIO_RGPIO_ECLK_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // When set, the gp_clk input signal is used to latch pin.
  };
} gpio_rgpio_eclk_t;

// RGPIO_ECLK.PINS - When set, the gp_clk input signal is used to latch pin.
#define GPIO_RGPIO_ECLK_PINS_WIDTH 32
#define GPIO_RGPIO_ECLK_PINS_LSB 0
#define GPIO_RGPIO_ECLK_PINS_MASK 0xffffffff
#define GPIO_RGPIO_ECLK_PINS_RESET 0x0

// RGPIO_NEC - Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.
#define GPIO_RGPIO_NEC_ADDR 0x24
#define GPIO_RGPIO_NEC_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t PINS : 32; // When set, gp_clk is active on negative edge, when cleared on positive edge.
  };
} gpio_rgpio_nec_t;

// RGPIO_NEC.PINS - When set, gp_clk is active on negative edge, when cleared on positive edge.
#define GPIO_RGPIO_NEC_PINS_WIDTH 32
#define GPIO_RGPIO_NEC_PINS_LSB 0
#define GPIO_RGPIO_NEC_PINS_MASK 0xffffffff
#define GPIO_RGPIO_NEC_PINS_RESET 0x0


// Register map structure
typedef struct {
    union {
        __I uint32_t RGPIO_IN; // Latched value of general-purpose input pins.
        __I gpio_rgpio_in_t RGPIO_IN_bf; // Bit access for RGPIO_IN register
    };
    union {
        __IO uint32_t RGPIO_OUT; // General-purpose output pin values.
        __IO gpio_rgpio_out_t RGPIO_OUT_bf; // Bit access for RGPIO_OUT register
    };
    union {
        __IO uint32_t RGPIO_OE; // General-purpose pins output enables.
        __IO gpio_rgpio_oe_t RGPIO_OE_bf; // Bit access for RGPIO_OE register
    };
    union {
        __IO uint32_t RGPIO_INTE; // General-purpose pin interrupt enables.
        __IO gpio_rgpio_inte_t RGPIO_INTE_bf; // Bit access for RGPIO_INTE register
    };
    union {
        __IO uint32_t RGPIO_PTRIG; // Trigger IRQ on positive edge.
        __IO gpio_rgpio_ptrig_t RGPIO_PTRIG_bf; // Bit access for RGPIO_PTRIG register
    };
    __IO uint32_t RESERVED0[1];
    union {
        __IO uint32_t RGPIO_CTRL_STATUS; // GPIO control and status register
        __IO gpio_rgpio_ctrl_status_t RGPIO_CTRL_STATUS_bf; // Bit access for RGPIO_CTRL_STATUS register
    };
    union {
        __IO uint32_t RGPIO_INTS; // GPIO interrupt status register.
        __IO gpio_rgpio_ints_t RGPIO_INTS_bf; // Bit access for RGPIO_INTS register
    };
    union {
        __IO uint32_t RGPIO_ECLK; // Latch on gp_clk input signal.
        __IO gpio_rgpio_eclk_t RGPIO_ECLK_bf; // Bit access for RGPIO_ECLK register
    };
    union {
        __IO uint32_t RGPIO_NEC; // Latch on gp_clk negative edge. Relevant only if RGPIO_ECLK is set.
        __IO gpio_rgpio_nec_t RGPIO_NEC_bf; // Bit access for RGPIO_NEC register
    };
} gpio_t;

#define GPIO ((gpio_t*)(GPIO_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __GPIO_REGS_H */
