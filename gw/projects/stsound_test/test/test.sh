#!/bin/bash

#This a simple bash based shell script testing the audio_dac model.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

rm -f pcm_out.py
rm -f test.wav

echo "Launching Vmodel..."
./Vmodel -s $SRC_ROOT_DIR/gw/projects/stsound_test/test/sdcard.img

if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
fi

REF_WAV="$SRC_ROOT_DIR/gw/projects/stsound_test/test/ref.wav"

echo "Launching python script analyzing model data..."
PYTHONPATH="." 
PYTHONPYCACHEPREFIX="."
$SRC_ROOT_DIR/gw/projects/stsound_test/test/stsound_test.py -r $REF_WAV

if [ "$?" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
else
  echo "Test Passed."
  exit 0
fi
