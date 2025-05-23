#ifndef DFX_CONTROLLER_HAL_H
#define DFX_CONTROLLER_HAL_H

#include <stdint.h>

/* Dynamic Function Exchange (DFX) register interface. */
/* The DFX Controller registers are documented in the Register Space section of */
/* the DFX Product Guide PG374. 
 * Example usage of this register interface can be found in
 * sw/projects/dfx_test/dfx_cli.cpp.*/

#define DFX_BASE 0x10000400

#define DFX_CONTROL_REG 0x0
//Register Layout:
#define DFX_CONTROL_HALFWORD_MASK 0xffff0000
#define DFX_CONTROL_HALFWORD_OFFSET 16
#define DFX_CONTROL_BYTE_MASK 0x0000ff00
#define DFX_CONTROL_BYTE_OFFSET 8
#define DFX_CONTROL_CMD_MASK 0x000000ff
#define DFX_CONTROL_CMD_SHUTDOWN 0
#define DFX_CONTROL_CMD_RESTART_NOT_STAT 1
#define DFX_CONTROL_CMD_RESTART_STAT 2
#define DFX_CONTROL_CMD_PROCEED 3
#define DFX_CONTROL_CMD_USR_CTRL 4

#define DFX_STATUS_REG 0x0
//Register Layout:
#define DFX_STATUS_RM_ID_MASK 0x00ffffff00
#define DFX_STATUS_RM_ID_OFFSET 8
#define DFX_STATUS_SHUTDOWN 0x00000080
#define DFX_STATUS_ERR_MASK 0x00000078
#define DFX_STATUS_ERR_OFFSET 3
#define DFX_STATUS_ERR_UNKNOWN 15
#define DFX_STATUS_ERR_BAD_FORMAT 8
#define DFX_STATUS_ERR_BAD_SIZE 7
#define DFX_STATUS_ERR_LOST_FETCH_ERR 6
#define DFX_STATUS_ERR_BS_FETCH_ERR 5
#define DFX_STATUS_ERR_FETCH_ERR 4
#define DFX_STATUS_ERR_LOST_ERR 3
#define DFX_STATUS_ERR_BS_ERR 2
#define DFX_STATUS_ERR_BAD_CONFIG 1
#define DFX_STATUS_ERR_NO_ERR 0
#define DFX_STATUS_STATE_MASK 0x00000007
#define DFX_STATUS_STATE_VS_FULL 7
#define DFX_STATUS_STATE_RM_RESET 6
#define DFX_STATUS_STATE_SW_STARTUP 5
#define DFX_STATUS_STATE_LOADING 4
#define DFX_STATUS_STATE_CLEARING 3
#define DFX_STATUS_STATE_SW_SHUTDOWN 2
#define DFX_STATUS_STATE_HW_SHUTDOWN 1
#define DFX_STATUS_STATE_VS_EMPTY 0

#define DFX_SW_TRIGGER_REG 0x4
//Register Layout:
#define DFX_SW_TRIGGER_PENDING 0x80000000
#define DFX_SW_TRIGGER_TRIG_ID 0x00000001

#define DFX_TRIGGER_0_REG 0x20
#define DFX_TRIGGER_1_REG 0x24

#define DFX_RM_BS_INDEX_0_REG 0x40
#define DFX_RM_BS_INDEX_1_REG 0x48
//Register Layout:
#define DFX_RM_BS_INDEX_MASK 0x0000ffff

#define DFX_RM_CONTROL_0_REG 0x44
#define DFX_RM_CONTROL_1_REG 0x4C
//Register Layout:
#define DFX_RM_CONTROL_RST_DURATION_MASK 0x00001fe0
#define DFX_RM_CONTROL_RST_DURATION_OFFSET 5
#define DFX_RM_CONTROL_RST_REQUIRED_MASK 0x00000018
#define DFX_RM_CONTROL_RST_REQUIRED_OFFSET 3
#define DFX_RM_CONTROL_STARTUP_REQUIRED 0x00000004
#define DFX_RM_CONTROL_SHUTDOWN_REQUIRED_MASK 0x00000003

#define DFX_BS_ID_0_REG 0x60
#define DFX_BS_ADDRESS_0_REG 0x64
#define DFX_BS_SIZE_0_REG 0x68
#define DFX_BS_ID_1_REG 0x70
#define DFX_BS_ADDRESS_1_REG 0x74
#define DFX_BS_SIZE_1_REG 0x78

#ifdef __cplusplus
extern "C" {
#endif

inline void dfx_reg_wr(uint32_t reg_offset, uint32_t data) {
  *(volatile uint32_t *)(DFX_BASE + reg_offset) = data;
}

inline uint32_t dfx_reg_rd(uint32_t reg_offset) {
  uint32_t res = *(uint32_t volatile *)(DFX_BASE + reg_offset);
  return res;
}

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

