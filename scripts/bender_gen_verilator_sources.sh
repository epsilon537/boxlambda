#! /bin/bash

#This script extracts the files to be verilated from the nearest bender.yml manifest and generates 
#a verilator script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <depfile target> <bl_target_fpga> <optional OOC target>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, containing verilator script
OUTFILE="$2"

# $3 = depfile target
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

bender -d $SRC_DIR update

bender -d $SRC_DIR script -t $BL_TARGET_FPGA $MIN_T_OOC verilator | tr '\n' ' ' > "$OUTFILE.tmp"

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No verilator source list changes detected."
else
  echo "Updating verilator source list".
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp

#If depfile target is given...
if [ -n "$DEPFILE_TGT" ]
then
  #Generate a depfile: Prepend each line with <target> :
  bender -d $SRC_DIR script $MIN_T_OOC -t verilator flist | sed "s#^#$DEPFILE_TGT \: #" > $OUTFILE.dep
fi