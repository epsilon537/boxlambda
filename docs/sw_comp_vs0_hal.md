## VS0 HAL

**VS0 HAL API**:
[boxlambda/sw/components/vs0_hal/vs0_hal.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/vs0_hal/vs0_hal.h)

The VS0 HAL contains definitions that are common across all VS0 RM variants (currently just *vs0_j1b* and *vs0_stub*):

- The base address and size of the VS0 RM address space.
- The VS0 Signature Register at the end of the VS0 RM address space. All VS0 RM variants are expected to implement this register and return a value that is unique to the RM. This allows software to identify the RM loaded into the RP before attempting to access RM-specific registers or memory. The *dfx_read_core_sig* command in the *dfx_cli* module reads this register.


