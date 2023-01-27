#! /bin/bash

#This script is used to synthesize and/or implement a design using Vivado.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir> synth|impl <part> <vivado script> <constraints script> <mem_files_script> <output dir> <top module>"
  exit 1
fi

#impl or synth command
CMD=$1

PART=$2

VIVADO_SCRIPT=$3

CONSTRAINTS_SCRIPT="$4"

MEM_FILES_SCRIPT="$5"

OUT_DIR=$6

TOP_MODULE=$7

BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/vivado.tcl \
	-notrace \
	-tclargs -project $OUT_DIR/project -cmd $CMD -part $PART \
	-sources $VIVADO_SCRIPT -constraints $CONSTRAINTS_SCRIPT \
	-mem_files $MEM_FILES_SCRIPT -outputDir $OUT_DIR -top $TOP_MODULE

