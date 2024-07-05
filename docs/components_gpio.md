## GPIO

- **GPIO Repo**, BoxLambda fork, *boxlambda* branch:
    [https://github.com/epsilon537/gpio](https://github.com/epsilon537/gpio)

- **GPIO Submodule in the BoxLambda Directory Tree**:
    boxlambda/sub/gpio/

- **GPIO Component in the BoxLambda Directory Tree**:
    [boxlambda/gw/components/gpio](https://github.com/epsilon537/boxlambda/tree/master/gw/components/gpio)

- **GPIO Core Top-Level**:
    [sub/gpio/rtl/verilog/gpio_top.v](https://github.com/epsilon537/gpio/blob/boxlambda/rtl/verilog/gpio_top.v)

- **GPIO Core Spec:**:
    [sub/gpio/rtl/doc/gpio_spec.pdf](https://github.com/epsilon537/gpio/blob/boxlambda/doc/gpio_spec.pdf)

The GPIO core comes from OpenCores.org. The authors are Damjan Lampret and Goran Djakovic. This core is very configurable and has a lot of features. Among other things:

- Inputs can be configured to trigger IRQs.
- Inputs can be configured to register at the rising edge of the system clock or either edge of an external clock (I have not not tested this feature yet).

BoxLambda instantiates the GPIO core with 24 pins. The pin assignment is given in the table below.

| GPIO  | Arty A7 Pin Assignment |
|-------|------------------------|
| 0     | LED 4                  |
| 1     | LED 5                  |
| 2     | LED 6                  |
| 3     | LED 7                  |
| 4     | Switch 0               |
| 5     | Switch 1               |
| 6     | Switch 2               |
| 7     | Switch 3               |
| 8     | Button 0               |
| 9     | Button 1               |
| 10    | Button 2               |
| 11    | Button 3               |
| 12-23 | ChipKit Pins 26-37     |
|       |                        |
| Ext. Clk. | ChipKit Pin 38     |

### GPIO Clock Frequency

The GPIO core is part of the 50MHz System Clock Domain.

