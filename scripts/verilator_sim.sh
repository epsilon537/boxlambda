#! /bin/bash

#This script verilates a design, i.e. it generates an Vmodel executable.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <verilator script> <vlts file> <vlt cpp files> <top_module> <outdir> <verilator flags>"
  exit 1
fi

VERILATOR_SCRIPT=`cat $1`
VLT_FILES=`cat $2`
VLT_CPP_FILES=`cat $3`
TOP_MODULE="$4"
OUTDIR="$5"

#Shift out previous args so we can capture remains args into FLAGS:
shift 5
FLAGS="$*"

verilator $FLAGS --no-timing --top-module $TOP_MODULE -Wall -cc --trace-fst --exe -x-assign 0 --build --prefix Vmodel --Mdir $OUTDIR/verilator $VLT_FILES $VLT_CPP_FILES $VERILATOR_SCRIPT && cp -f $OUTDIR/verilator/Vmodel $OUTDIR/Vmodel
