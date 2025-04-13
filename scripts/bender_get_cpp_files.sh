#! /bin/bash

#This script extracts just the .c/.cpp files to be verilated from the nearest bender.yml manifest and writes
#those file paths in an output file.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file containing the cpp file list
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

#Get all files from bender verilator target, filter out the cpp and c file, and put everything on one line,
bender -d $SRC_DIR script flist -t $BL_TARGET_FPGA -t verilator | grep "\.cpp$\|\.c$" | tr '\n' ' ' > "$OUTFILE"

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

