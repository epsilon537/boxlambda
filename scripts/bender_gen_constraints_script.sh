#! /bin/bash

#This script extracts the constraints file from the nearest bender.yml manifest and generates a tcl subscript
#to be included in the main vivado.tcl script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir> <outfile>"
  exit 1
fi

# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file, containing constraints tcl script for vivado
OUTFILE="$2"

rm -f $OUTFILE

bender -d $SRC_DIR update

CONSTRAINTS=`bender -d $SRC_DIR script flist -n -t constraints`

for constraint in $CONSTRAINTS
do
    echo add_files -fileset constrs_1 $constraint >> $OUTFILE
done
