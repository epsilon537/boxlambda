#! /bin/bash

#This script is used to synthesize and/or implement a design using Vivado.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <part> <vivado script> <constraints script> <mem_files_script> <output dir> <top module>"
  exit 1
fi

PART=$1

SOURCES_SCRIPT=$2

CONSTRAINTS_SCRIPT="$3"

MEM_FILES_SCRIPT="$4"

OUT_DIR=$5

TOP_MODULE=$6

BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/vivado_create_project.tcl \
	-notrace \
	-tclargs -project $OUT_DIR/project -part $PART \
	-sources $SOURCES_SCRIPT -constraints $CONSTRAINTS_SCRIPT \
	-mem_files $MEM_FILES_SCRIPT -outputDir $OUT_DIR -top $TOP_MODULE

if [ $? == 0 ]
then
	touch project.dep
fi
