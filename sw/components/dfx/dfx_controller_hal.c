#include "dfx_controller_hal.h"
#include <assert.h>
#include <stdint.h>
#include "timer.h"

/* DFX load a reconfigurable module from memory into
 * Socket 0 (VS0). This function combines a number of register level
 * interactions with the DFX controller.
 * bufPtr: pointer to memory buffer holding the RM bitstream.
 * size: size of the bitstream.
 * timeout_ms: number of milliseconds to wait for DFX Controller state to reach
 * DFX_STATUS_STATE_VS_FULL after loading has been initiated.
 * Returns 0 if successful, a negative value on error.
 */
uint32_t dfx_load_rm(void *bufPtr, uint32_t size, uint32_t timeout_ms) {
  uint32_t addr;

  addr = (uint32_t)bufPtr;
  assert(addr);

  /* Temporarily shut down the DFX Controller so we can write to the DFX BS INFO 0 register. */
  DFX_CTRL->CONTROL_bf.CMD = DFX_CTRL_CONTROL_CMD_SHUTDOWN;

  /* Point BS_INFO register 0 to the memory buffer holding the module */
  DFX->BS_ADDRESS_0 = addr;
  DFX->BS_SIZE_0 = size;

  /* Turn the DFX Controller back on */
  DFX_CTRL->CONTROL_bf.CMD = DFX_CTRL_CONTROL_CMD_RESTART_NO_STAT;

  /* Issue a trigger (index 0) to kick off the loading of the module into the virtual socket*/
  DFX->SW_TRIGGER_bf.TRIGGER_ID = 0;

  /* Check the state */
  dfx_status_t status_reg;
  uint32_t err=0;
  uint32_t state=0, prev_state=0;

  uint64_t endTime = mtimer_get_raw_time() + MTIMER_MSEC_TO_CLOCKS(timeout_ms);

  /* The DFX controller is going to cycle through a few states and, hopefully, end up in the VS_FULL state. */
  while (state != DFX_STATUS_STATE_VS_FULL) {
    status_reg = DFX->STATUS_bf;
    state = status_reg.STATE;
    err = status_reg.ERR;

    //Abort if we detecte an error.
    if (err != DFX_STATUS_ERR_NO_ERR) {
      return -1;
    }

    //Abort on timeout
    if (mtimer_get_raw_time() >= endTime) {
      return -2;
    }

    if (state != prev_state) {
      prev_state = state;
    }
  }

  return 0;
}
