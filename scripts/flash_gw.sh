#! /bin/bash

#This script flashes the give GW bitsream file to an Arty A7 target.

if [[ "$#" < 2  || "$1" == "-h" ]]
then
  echo "$0 arty_a7_[35|100]t <bitstream file>"
  exit 1
fi

BOARD=$1
BITSTREAM_IN=$2

openFPGALoader -b $BOARD -f $BITSTREAM_IN
openFPGALoader -b $BOARD -r

