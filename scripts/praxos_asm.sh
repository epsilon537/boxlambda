#! /bin/bash

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <pxasm input file>"
  exit 1
fi

export PYTHONDONTWRITEBYTECODE=1

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

python3 $SCRIPT_DIR/../sub/Praxos/sw/src/praxos_asm.py -p 256 -s $1
