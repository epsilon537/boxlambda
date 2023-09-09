#!/bin/bash

#! /bin/bash

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <pxasm input file>"
  exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )/../../../../scripts/"

export PYTHONDONTWRITEBYTECODE=1
export PYTHONPATH=".:$SCRIPT_DIR:$PYTHONPATH"

python3 -B $SCRIPT_DIR/../sub/Praxos/sw/src/praxos_asm.py -p 256 -s $1 -o praxos_prog
if [ $? -eq 0 ] 
then
  `cocotb-config --python-bin` -B praxos_monitor.py
fi
