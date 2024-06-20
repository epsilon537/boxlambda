#!/bin/bash

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir>"
  exit 1
fi

SRC_ROOT_DIR="$1"

./Vmodel

# Workaround for newline not working in terminal after running Vmodel.
# This has something to do with this VModel being SDL and/or curses based.
reset

RES="$?"
echo "RES=$RES"
if [ "$RES" -ne "0" ]; then
  echo "Test Failed!"
  exit -1
fi

echo "Checking frame file..."
if cmp --silent -- frame.bin $SRC_ROOT_DIR/gw/projects/vera_integrated/test/frame.ref; then
  echo "Test Passed."
  exit 0;
else
  echo "Test Failed!"
  exit -1
fi
