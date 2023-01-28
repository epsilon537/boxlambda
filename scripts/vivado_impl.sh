#! /bin/bash

#This script is used to synthesize and/or implement a design using Vivado.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <output dir>"
  exit 1
fi

OUT_DIR=$1
BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/vivado_impl.tcl -notrace -tclargs -project $OUT_DIR/project -outputDir $OUT_DIR

