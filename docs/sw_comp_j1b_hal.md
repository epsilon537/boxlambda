## J1B HAL

**J1B HAL API**:
[boxlambda/sw/components/j1b_hal/j1b_hal.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/j1b_hal/j1b_hal.h)

The J1B Hardware Access Layer gives access to the J1B demo core's register interface and includes a function for loading the firmware into the core. The j1b_test program uses the j1b_hal to perform the following:

1. Boot the SwapForth firmware.
2. Forward the BoxLambda serial port input data to the J1B core UART input register.
3. Forward J1B core UART output data to the BoxLambda serial port.

This way, the SwapForth REPL running on the J1B core is presented to the user on the serial port terminal.
