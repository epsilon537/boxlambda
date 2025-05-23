#! /bin/bash

#This script extracts the .vlt lint files from the nearest bender.yml manifest and writes
#those file paths in an output file.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file containing the .vlt file list
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

#BASEDIR is the location of the current script, i.e. the scripts directory
BASEDIR=$(dirname "$0")

#Get all files from bender verilator target, filter out the .vlt files, and put everything on one line,
BENDER_VLT=`bender -d $SRC_DIR script flist -t BL_TARGET_FPGA -t verilator | grep ".vlt$" | tr '\n' ' '`

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

#'return' the default vlt and the lint.vlt retrieved from the bender manifest (if it has one).
echo "$BASEDIR/lint_default.vlt $BENDER_VLT" > "$OUTFILE"


