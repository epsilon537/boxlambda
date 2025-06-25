#ifndef DFX_CONTROLLER_HAL_H
#define DFX_CONTROLLER_HAL_H

/* The DFX Controller registers are documented in the Register Space section of */
/* the DFX Product Guide PG374.
 * Example usage of this register interface can be found in
 * sw/projects/dfx_test/dfx_cli.cpp.*/
/* The DFX Controller control register is in a separate file due to a Corsair
 * limitation. The control register is a write-only register located at the
 * same address as the status register.*/
#include "dfx_controller_ctrl_reg.h"
/* All other DFX Controller registers are defined in this file:*/
#include "dfx_controller_other_regs.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* DFX load a reconfigurable module from memory into
 * Socket 0 (VS0). This function combines a number of register level
 * interactions with the DFX controller.
 * bufPtr: pointer to memory buffer holding the RM bitstream.
 * size: size of the bitstream.
 * timeout_ms: number of milliseconds to wait for DFX Controller state to reach
 * DFX_STATUS_STATE_VS_FULL after loading has been initiated.
 * Returns 0 if successful, a negative value on error.
 */
uint32_t dfx_load_rm(void *bufPtr, uint32_t size, uint32_t timeout_ms);

#ifdef __cplusplus
}
#endif
#endif /*DFX_CONTROLLER_HAL_H*/

