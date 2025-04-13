#! /bin/bash

# This script is used by the build system. It does the following:
# - extract the files to be synthesized from the nearest bender.yml manifest
# - generate a tcl subscript to be included in the main vivado_create_project.tcl script

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga> <optional OOC target>"
  exit 1
fi

# $1 = source directory
SRC_DIR="$1"

# $2 = output file, a vivado tcl script
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

# $4 = optional OOC target
#-z tests for empty string.
if [ -z "$4" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$4"
fi

bender -d $SRC_DIR script -t $BL_TARGET_FPGA $MIN_T_OOC vivado > $OUTFILE

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

