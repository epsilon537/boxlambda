## Miscellaneous Modules

- **DFX Controller** (*not yet implemented*): The actual loading of a Reconfigurable Module into a Reconfigurable Partition is handled by the DFX Controller. DFX stands for **Dynamic Function Exchange** which is Xilinx-speak for Partial FPGA Reconfiguration.
- **ICAP** (*not yet implemented*): Internal Configuration Access Port. This module gives access to the FPGA configuration functionality built into Xilinx FPGAs. We'll use the ICAP to implement in-system updates of the Full Configuration Bitstream, loaded into the FPGA upon boot-up.
- **Quad SPI Flash** (*not yet implemented*): This is a module provided by Xilinx, giving access to the Flash Memory device attached through a Quad-SPI bus. The non-volatile Flash Memory will hold the Full Configuration Bitstream(s), System Firmware, and non-volatile system configuration parameters such as keyboard type
- **wb_gpio**: A simple GPIO core with a Wishbone interface, for sampling buttons and switches, and driving LEDs. [Wb_gpio.sv](https://github.com/epsilon537/ibex_wb/blob/001f5ee9253a8eefc9bb1db05ebfb3887d314c58/soc/fpga/arty-a7/rtl/wb_gpio.sv) is included in the [Ibex_WB](https://github.com/epsilon537/ibex_wb) submodule.

