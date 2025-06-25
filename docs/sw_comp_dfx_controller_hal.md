---
hide:
  - toc
---

## DFX Controller HAL

**DFX Controller HAL**:
[boxlambda/sw/components/dfx/dfx_controller_hal.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/dfx/dfx_controller_hal.h)

The DFX Controller HAL provides software access to the DFX Controller's register interface. It also includes a helper function to load a Reconfigurable Module from memory into Virtual Socket 0 (VS0). This function handles several register-level interactions with the DFX controller.

### The DFX Controller Register Interface

Due to a limitation of the Corsair tool, the DFX Controller register interface is split across two header files,
[dfx_controller_ctrl_reg.h](https://github.com/epsilon537/boxlambda/blob/master/registers/generated/dfx_controller_ctrl_reg.h) and [dfx_controller_other_regs.h](https://github.com/epsilon537/boxlambda/blob/master/registers/generated/dfx_controller_other_regs.h). The write-only Control
register and the read-only Status register are located at the same address.
Corsair doesn't support two register definitions at the same address. The
workaround is to define the two registers in separate files.
