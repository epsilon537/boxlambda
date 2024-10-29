#! /bin/bash

# This script is used by the build system. It does the following:
# - extract the files to be synthesized from the nearest bender.yml manifest 
# - generate a tcl subscript to be included in the main vivado_create_project.tcl script
# - generate a dependency file list.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <depfile target> <bl_target_fpga> <optional OOC target>"
  exit 1
fi

# $1 = source directory
SRC_DIR="$1"

# $2 = output file, a vivado tcl script
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

bender -d $SRC_DIR update
bender -d $SRC_DIR script -t $BL_TARGET_FPGA $MIN_T_OOC vivado > $OUTFILE.tmp

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No vivado source list changes detected."
else
  echo "Updating vivado source list".
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp

#Generate a depfile: Prepend each line with <target> :
bender -d $SRC_DIR script $MIN_T_OOC -t $BL_TARGET_FPGA -t prj_constraints -t dfx_constraints -t vivado flist | sed "s#^#$DEPFILE_TGT \: #" > $OUTFILE.dep

