#!/bin/bash

#Launch the verilator model
./Vmodel &
sleep 3

#Launch riscv-openocd and connect to the model
openocd -f ../sim/dm_debug.cfg &
sleep 3

#Launch gdb, connect to target via openocd, execute a via debug actions as indicated in test.gdb script.
#Collect result of actions in gdb.log
rm -f gdb.log
riscv32-unknown-elf-gdb --batch -x ../sim/test.gdb ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.elf > gdb.log

#Check if log contains given output, confirming we had a valid connection with the target.
grep "$1 = 0x10010000" gdb.log
if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
