#! /bin/bash

#This script extracts the files to be verilated from the nearest bender.yml manifest and generates a yosys script.
#This script is used by the build system.

# This script is used by the build system. It does the following:
# - extract the files to be synthesized from the nearest bender.yml manifest 
# - generate a filelist to be used by the yosys synth script. 
#   The filelist is only updated when there actually are
#   changes in the filelist so it can be used by the build system for dependency checking.
# - generate a dependency file list.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <depfile target> <bl_target_fpga> <optional OOC target>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, containing yosys script
OUTFILE="$2"

# $3 = target to be used in the generated
DEPFILE_TGT="$3"

# $4 = BL_TARGET_FPGA
BL_TARGET_FPGA="$4"

# $5 = optional OOC target
#-z tests for empty string.
if [ -z "$5" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$5"
fi

bender -d $SRC_DIR script -t $BL_TARGET_FPGA $MIN_T_OOC -t openxc7 flist-plus > $OUTFILE.tmp

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No yosys source list changes detected."
else
  echo "Updating yosys source list".
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp

#Generate a depfile: Prepend each line with <target> :
bender -d $SRC_DIR script $MIN_T_OOC -t $BL_TARGET_FPGA -t openxc7 flist | sed "s#^#$DEPFILE_TGT \: #" > $OUTFILE.dep

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

