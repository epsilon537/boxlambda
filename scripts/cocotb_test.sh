#!/bin/bash

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <cocotb python test script>"
  exit 1
fi

PYTHON_SCRIPT="$1"
export PYTHONPATH="."
export PYTHONDONTWRITEBYTECODE=1

`cocotb-config --python-bin` -B $PYTHON_SCRIPT
