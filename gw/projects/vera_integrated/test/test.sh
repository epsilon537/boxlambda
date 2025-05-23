#!/bin/bash

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

./Vmodel

RES="$?"
echo "RES=$RES"
if [ "$RES" -ne "0" ]; then
  echo "Vmodel returned unsuccessfully. Test Failed!"
  exit -1
else
  echo "Vmodel returned successfully."
fi

echo "Checking frame file..."
if cmp --silent -- frame.bin $SRC_ROOT_DIR/gw/projects/vera_integrated/test/frame.ref; then
  echo "Frame ref matches. Test Passed."
  exit 0;
else
  echo "Frame ref comparison failed. Test Failed!"
  exit -1
fi
