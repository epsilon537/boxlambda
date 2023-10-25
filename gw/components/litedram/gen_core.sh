#! /bin/sh

#Invoke Litex's liteDRAM generator script twice, once for FPGA and once for simulation
#The generated code is checked in. The cores only need to be regenerated when the LiteDRAM YML configuration
#file changes.
litedram_gen artya7dram.yml --sim --gen_user_clkx2 --gateware-dir sim/rtl --software-dir sim/sw --name litedram
litedram_gen artya7dram.yml --gen_user_clkx2 --gateware-dir arty/rtl --software-dir arty/sw --name litedram