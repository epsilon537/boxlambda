#!/bin/bash

#This wrapper shell scripts sets up the environment for running a CocoTB test script.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <cocotb python test script>"
  exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
echo "Script directory: $SCRIPT_DIR"
PYTHON_SCRIPT="$1"
echo "Test script: $PYTHON_SCRIPT"
export PYTHONPATH=".:$SCRIPT_DIR:$PYTHONPATH"
echo "PYTHONPATH: $PYTHONPATH"
export PYTHONDONTWRITEBYTECODE=1

`cocotb-config --python-bin` -B $PYTHON_SCRIPT
