#! /bin/sh

#Invoke Litex's liteDRAM generator script for FPGA, Verilator, and OpenXC7
#The generated code is checked in. The cores only need to be regenerated when the LiteDRAM YML configuration
#file changes.
litedram_gen 50mhz.yml --sim --gen_user_clkx2 --gateware-dir sim_50mhz/rtl --software-dir sim_50mhz/sw --name litedram
litedram_gen 50mhz.yml --gen_user_clkx2 --gateware-dir fpga_50mhz/rtl --software-dir fpga_50mhz/sw --name litedram
litedram_gen 25mhz.yml --sim --gen_user_clkx2 --gateware-dir sim_25mhz/rtl --software-dir sim_25mhz/sw --name litedram
litedram_gen 25mhz.yml --gen_user_clkx2 --gateware-dir fpga_25mhz/rtl --software-dir fpga_25mhz/sw --name litedram

