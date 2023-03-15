#! /bin/bash

#This script extracts the files to be synthesized from the nearest bender.yml manifest and generates 
#a tcl sub script to be included in the main vivado.tcl script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <depfile target> <optional OOC target>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, a vivado tcl script
OUTFILE="$2"

# $3 = depfile target
DEPFILE_TGT="$3"

# $4 = optional OOC target
#-z tests for empty string.
if [ -z "$4" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$4"
fi

bender -d $SRC_DIR update
bender -d $SRC_DIR script $MIN_T_OOC vivado > $OUTFILE.tmp

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No vivado source list changes detected."
else
  echo "Updating vivado source list".
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp

#Generate a depfile: Prepend each line with <target> :
bender -d $SRC_DIR script $MIN_T_OOC -t constraints -t vivado flist | sed "s#^#$DEPFILE_TGT \: #" > $OUTFILE.dep
