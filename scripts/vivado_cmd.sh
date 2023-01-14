#! /bin/sh

#impl or synth command
CMD=$1

PART=$2

VIVADO_SCRIPT=$3

CONSTRAINTS_SCRIPT="$4"

#if [ -z $4 ]
#then
#    CONSTRAINTS_SCRIPT=""
#else
#    CONSTRAINTS_SCRIPT="-constraints `cat $4`"
#fi

MEM_FILES_SCRIPT="$5"

#if [ -z $5 ]
#then
#    MEM_FILES_SCRIPT=""
#else
#    MEM_FILES_SCRIPT="-mem_files `cat $5`"
#fi

OUT_DIR=$6

TOP_MODULE=$7

BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/vivado.tcl \
	-tclargs -project $OUT_DIR/project -cmd $CMD -part $PART \
	-sources $VIVADO_SCRIPT -constraints $CONSTRAINTS_SCRIPT \
	-mem_files $MEM_FILES_SCRIPT -outputDir $OUT_DIR -top $TOP_MODULE
