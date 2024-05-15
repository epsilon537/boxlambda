#! /bin/bash

#This script converts a bitstream file into a .bin
#file that can be flashed onto the target.

if [[ "$#" < 1  || "$1" == "-h" ]]
then
  echo "$0 <bitstream input file>"
  exit 1
fi

BITSTREAM_IN=$1

BITSTREAM_ABS_IN=`realpath $BITSTREAM_IN`

echo "all:" > bif.bif
echo "{" >> bif.bif
echo $BITSTREAM_ABS_IN >> bif.bif
echo "}" >> bif.bif

bootgen -image bif.bif -arch fpga -o $BITSTREAM_ABS_IN.bin -w on
