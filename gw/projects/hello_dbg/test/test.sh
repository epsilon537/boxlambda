#!/bin/bash

#This a simple bash based shell script verifying OpenOCD based JTAG connectivity to a riscv32 target running 'Hello Word'.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

#Launch the verilator model with -d option to attach debugger.
echo "Launching Vmodel..."
./Vmodel -d &
sleep 3

#Launch riscv-openocd and connect to the model
openocd -f $SRC_ROOT_DIR/scripts/verilator_riscv_dbg.openocd.cfg &
sleep 3

#Launch gdb, connect to target via openocd, execute a via debug actions as indicated in test.gdb script.
#Collect result of actions in gdb.log
rm -f gdb.log
echo "Launching gdb..."
riscv32-unknown-elf-gdb --batch -x $SRC_ROOT_DIR/gw/projects/hello_dbg/test/test.gdb ../../../sw/projects/hello_world/hello_world > gdb.log

#Check if log contains given output, confirming we had a valid connection with the target.
#When we reach this point, the SW running on target should be in its final state, which is an infinite loop. In this state,
#the UART register contents (retrieved with GDB) should be 0x10010000.
echo "Checking gdb log..."
cat gdb.log
grep "\$1 = 0x10010000" gdb.log
RES="$?"
echo "RES=$RES"
if [ "$RES" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
