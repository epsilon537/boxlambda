#! /bin/bash

#This script extracts the files to be verilated from the nearest bender.yml manifest and generates
#a verilator script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga> <optional OOC target>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, containing verilator script
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

# $4 = optional OOC target
#-z tests for empty string.
if [ -z "$4" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t $4"
fi

bender -d $SRC_DIR script -t $BL_TARGET_FPGA $MIN_T_OOC verilator | tr '\n' ' ' > "$OUTFILE"

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

