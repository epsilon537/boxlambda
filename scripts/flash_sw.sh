#! /bin/bash

#This script flashes the give SW image to an Arty A7 target
#at the offset expected by BoxLambda.

if [[ "$#" < 2  || "$1" == "-h" ]]
then
  echo "$0 arty_a7_[35|100]t <.bin input file>"
  exit 1
fi

BOARD=$1
BIN_IN=$2

openFPGALoader -b $BOARD -f -o 4194304 $BIN_IN

