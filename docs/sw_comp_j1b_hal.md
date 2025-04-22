---
hide:
  - toc
---

## J1B HAL

**J1B HAL API**:
[boxlambda/sw/components/j1b_hal/j1b_hal.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/j1b_hal/j1b_hal.h)

The J1B Hardware Access Layer (HAL) provides access to the J1B demo core's register interface and includes a function for loading firmware into the core. The [dfx_test](https://github.com/epsilon537/boxlambda/tree/master/sw/projects/dfx_test) program uses the J1B HAL to perform the following tasks:

1. Boot the SwapForth firmware.
2. Forward input data from the BoxLambda serial port to the J1B core UART input register.
3. Forward UART output data from the J1B core to the BoxLambda serial port.

This allows the SwapForth REPL running on the J1B core to be presented to the user on the serial port terminal.
