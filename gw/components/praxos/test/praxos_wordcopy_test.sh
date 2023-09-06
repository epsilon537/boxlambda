#!/bin/bash

#This a simple bash based shell script testing the audio_dac model.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

echo "Assembling praxos program..."
$SRC_ROOT_DIR/scripts/praxos_asm.sh $SRC_ROOT_DIR/gw/components/praxos/test/praxos_wordcopy.pxasm praxos_wordcopy_prog

if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
fi

echo "Launching cocotob python script analyzing model data..."
$SRC_ROOT_DIR/scripts/cocotb_test.sh $SRC_ROOT_DIR/gw/components/praxos/test/praxos_wordcopy_test.py
if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
