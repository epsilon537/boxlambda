#ifndef J1B_HAL_H
#define J1B_HAL_H

#include <assert.h>
#include "vs0_hal.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Hardware Access Layer for the J1B DFX demo component.
 *
 * J1B is a tiny 32-bit stack processor designed to run Forth. J1B is part of
 * the SwapForth environment.
 *
 * For example usage of this HAL, see j1b_cli.cpp and/or j1b_test.cpp.
 */

/* The component inherits its base address from the abstract VS0 HAL.
 * VS0 = Virtual Socket 0, the gateware socket where the J1B components plugs into.
 */
#define J1B_BASE VS0_BASE

//Program Memory Base Address and size
#define J1B_PROG_MEM_BASE J1B_BASE
#define J1B_PROG_SIZE_BYTES 32768

//Registers Base Address.
#define J1B_REG_BASE (J1B_BASE+J1B_PROG_SIZE_BYTES)

//Output IRQ register offset
#define J1B_REG_IRQ_OUT 0
//Input IRQ register offset
#define J1B_REG_IRQ_IN 1

//Control register offset
#define J1B_REG_CTRL 2
//Register Layout:
#define J1B_REG_CTRL_RST_N 1

//UART TX-to-J1B register offset
#define J1B_REG_UART_TX_TO_J1B 16
//Register Layout:
#define J1B_REG_UART_TX_TO_J1B_DATA_WAITING 0x100

#define J1B_REG_GP_TO_J1B 17

//UART RX-from-J1B register offset
#define J1B_REG_UART_RX_FROM_J1B 18
//Register Layout:
#define J1B_REG_UART_RX_FROM_J1B_DATA_AVL 0x100
#define J1B_REG_UART_RX_FROM_J1B_RX_DATA_MSK 0xff

//Read general purpose register from J1B.
#define J1B_REG_GP_FROM_J1B 19

//The J1B component signature value, used to confirm that the J1B component is
//activate in the system.
//The register to read the signature value from is VS0_REG_SIGNATURE, part
//of vs0_hal.h.
#define J1B_SIG_VALUE 0xf041011b

#define J1B_REG_MAX J1B_REG_GP_FROM_J1B

//Write to Register
inline void j1b_reg_wr(unsigned reg_offset, unsigned val)
{
	assert(reg_offset <= J1B_REG_MAX);
	((unsigned volatile *)(J1B_REG_BASE))[reg_offset] = val;
}

//Read from Register
inline unsigned j1b_reg_rd(unsigned reg_offset)
{
	assert(reg_offset <= J1B_REG_MAX);
	return ((unsigned volatile *)(J1B_REG_BASE))[reg_offset];
}

//Load a j1b program into the J1B core's Program Memory.
//progLen must be a multiple of 4 and <= J1B_PROG_SIZE_BYTES.
void j1b_load_program(unsigned char *progData, unsigned progLen);

#ifdef __cplusplus
}
#endif

#endif //J1B_HAL_H
