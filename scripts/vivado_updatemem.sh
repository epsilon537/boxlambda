#! /bin/bash

#This script updates the SW memory image in the FPGA bitstream file
#The .mmi file is generating by Vivado during synthesis.

if [[ "$#" < 4  || "$1" == "-h" ]]
then
  echo "$0 <mmi file> <mem file> <bitstream input file> <bitstream output file>"
  exit 1
fi

MMI_FILE=$1
MEM_FILE=$2
BITSTREAM_IN=$3
BITSTREAM_OUT=$4

#Hack: It is assumed that this is the memory instance that needs updating
PROC=wb_spram/xpm_memory_spram_inst/xpm_memory_base_inst 
updatemem --force --meminfo $MMI_FILE --data $MEM_FILE --bit $BITSTREAM_IN --proc $PROC --out $BITSTREAM_OUT
