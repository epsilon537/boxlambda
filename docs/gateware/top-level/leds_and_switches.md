# LEDs

LEDs 0-3 have a fixed role in BoxLambda:

| LED # | Name | Description |
|-------|------|-------------|
| 0 | **PLL locked** | Lights up blue when the system clock PLL is locked. |
| 1 | **Init Done** | Lights up blue when SDRAM initialization has been completed. |
| 2 | **Init Error** | Lights up red when the SDRAM Memory Controller detects an error. |
| 3 | **SD Card Detect** | Lights up green when an SD card is detected in the SD PMOD card slot. |

LEDs 4-7 are assigned to the GPIO core. See the [GPIO component](components_gpio.md) page.

# Switches

- If switches 0-3 are all in the off position, the bootloader will spin after
loading the application image, allowing a debugger to connect.

- If switches 0-3 are all in the on position, test applications may treat that as
an indication that the application is running in a Verilator simulation environment.
