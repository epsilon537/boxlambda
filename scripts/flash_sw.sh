#! /bin/bash

#This script flashes the give SW image to an Arty A7 target
#at the offset expected by BoxLambda.

if [[ "$#" < 2  || "$1" == "-h" ]]
then
  echo "$0 [-b] arty_a7_[35|100]t <.bin input file>"
  exit 1
fi

if [[ "$1" == "-b" ]]
then
  #Bootloader image lives at flash offset 0x500000
  ADDR=5242880
  shift
else
  #Application image lives at 0x600000
  ADDR=6291456
fi

BOARD=$1
BIN_IN=$2

openFPGALoader -b $BOARD -f -o $ADDR $BIN_IN

