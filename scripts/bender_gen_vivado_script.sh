#! /bin/bash

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir> <outfile>"
  exit 1
fi

# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file, a vivado tcl script
OUTFILE="$2"

# $3 = optional OOC target
#-z tests for empty string.
if [ -z "$3" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$3"
fi

bender -d $SRC_DIR update
bender -d $SRC_DIR script $MIN_T_OOC vivado > $OUTFILE