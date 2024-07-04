#ifndef GPIO_REGS_H
#define GPIO_REGS_H

#ifdef __cplusplus
extern "C" {
#endif

#define GPIO_BASE 0x10000100

/*
 * GPIO Hardware Access Layer
 */

//GPIO registers
#define GPIO_RGPIO_IN      0
#define GPIO_RGPIO_OUT     1
#define GPIO_RGPIO_OE      2
#define GPIO_RGPIO_INTE    3
#define GPIO_RGPIO_PTRIG   4

#define GPIO_RGPIO_CTRL    6
#define GPIO_RGPIO_INTS    7

#define GPIO_RGPIO_ECLK   8
#define GPIO_RGPIO_NEC    9

// RGPIO_CTRL bit masks
#define GPIO_RGPIO_CTRL_INTE_MSK    (1<<0)
#define GPIO_RGPIO_CTRL_INTS_MSK    (1<<1)

#ifdef __cplusplus
}
#endif

#endif //GPIO_REGS_H
