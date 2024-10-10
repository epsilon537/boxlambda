#! /bin/bash

#This script extracts the prj_constraints file from the nearest bender.yml manifest and generates a tcl subscript
#to be included in the main vivado.tcl script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, containing prj_constraints tcl script for vivado
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

bender -d $SRC_DIR update

PRJ_CONSTRAINTS=`bender -d $SRC_DIR script flist -n -t prj_constraints`

for constraint in $PRJ_CONSTRAINTS
do
    echo add_files -fileset constrs_1 $constraint >> $OUTFILE.tmp
done

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No constraint filelist changes detected."
else
  echo "Updating constraint filelist".
  cp $OUTFILE.tmp $OUTFILE
fi

rm -f $OUTFILE.tmp
