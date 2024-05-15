#! /bin/bash

#This script loads the give GW bitstream file to an Arty A7 target.

if [[ "$#" < 2  || "$1" == "-h" ]]
then
  echo "$0 arty_a7_[35|100]t <bitstream file>"
  exit 1
fi

BOARD=$1
BITSTREAM_IN=$2

openFPGALoader -b $BOARD $BITSTREAM_IN

