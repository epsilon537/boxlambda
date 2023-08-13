#!/bin/bash

#This a simple bash based shell script testing the audio_dac model.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

echo "Launching Vmodel..."
./Vmodel 

if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
fi

echo "Launching python script analyzing model data..."
export PYTHONPYCACHEPREFIX="." #To make sure no pycache files are generated in the source tree.
export PYTHONPATH="." 
$SRC_ROOT_DIR/gw/projects/audio_dac_test/test/dac_test.py

if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
