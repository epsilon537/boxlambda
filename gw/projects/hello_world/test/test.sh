#!/bin/bash

#This a simple bash based shell script verifying OpenOCD based JTAG connectivity to a riscv32 target running 'Hello Word'.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

#Launch the verilator model with -a option to attach debugger.
echo "Launching Vmodel..."
./Vmodel -a &
sleep 3

#Record Vmodel's process id so we can use it later to kill the process
VMODEL_PROCESS_ID=`jobs -p`

#Launch riscv-openocd and connect to the model
openocd -f $SRC_ROOT_DIR/scripts/verilator.openocd.cfg &
sleep 3

#Launch gdb, connect to target via openocd, execute a via debug actions as indicated in test.gdb script.
#Collect result of actions in gdb.log
rm -f gdb.log
echo "Launching gdb..."

#If gdb-multiarch exists, use it.
if which gdb-multiarch ; then
  echo "gdb-multiarch found."
  export GDB=gdb-multiarch
else
  echo "gdb-multiarch not found. Trying gdb..."
  #If it doesn't, assume the gdb executable has multiarch support.
  #E.g. on Arch Linux this is the case.
  export GDB=gdb
fi

$GDB --batch -x $SRC_ROOT_DIR/gw/projects/hello_world/test/test.gdb ../../../sw/projects/hello_world/hello_world_ram > gdb.log

#Kill the Vmodel process so it doesn't linger forever in the background.
kill -9 $VMODEL_PROCESS_ID

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
