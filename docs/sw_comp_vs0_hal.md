---
hide:
  - toc
---

# VS0 HAL Overview

The **VS0 HAL API** provides common definitions for all VS0 RM variants, currently including `vs0_j1b` and `vs0_stub`. You can find the API here: [boxlambda/sw/components/vs0_hal/vs0_hal.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/vs0_hal/vs0_hal.h).

## Key Features:

- **VS0 RM Address Space**:
       - Specifies the base address and size of the address space allocated to the VS0 RM.

- **VS0 Signature Register**:
       - Located at the end of the VS0 RM address space.
       - Must be implemented by all VS0 RM variants to provide a unique identifier for the RM.
       - Helps software identify the RM loaded in the RP before interacting with any RM-specific components.
       - The `dfx_read_core_sig` command from the `dfx_cli` module reads this register.

