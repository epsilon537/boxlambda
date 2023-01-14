#!/bin/bash

#This a simple bash based shell script verifying OpenOCD based JTAG connectivity to a riscv32 target running 'Hello Word'.

#Launch the verilator model with -d option to attach debugger.
./Vmodel -d &
sleep 3

#Launch riscv-openocd and connect to the model
openocd -f ../../../openocd/verilator_riscv_dbg.cfg &
sleep 3

#Launch gdb, connect to target via openocd, execute a via debug actions as indicated in test.gdb script.
#Collect result of actions in gdb.log
rm -f gdb.log
riscv32-unknown-elf-gdb --batch -x ../test/test.gdb ../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello/hello.elf > gdb.log

#Check if log contains given output, confirming we had a valid connection with the target.
#When we reach this point, the SW running on target should be in its final state, which is an infinite loop. In this state,
#the UART register contents (retrieved with GDB) should be 0x10010000.
grep "$1 = 0x10010000" gdb.log
if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
