#! /bin/bash

#This script verilates a design, i.e. it generates an Vmodel executable.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <verilator script> <vlts file> <vlt cpp files> <top_module>"
  exit 1
fi

VERILATOR_SCRIPT=`cat $1`
VLT_FILES=`cat $2`
VLT_CPP_FILES=`cat $3`
TOP_MODULE="$4"

if [ -z "$5" ]
then
    CFLAGS=""
else
    CFLAGS="$5"
fi

if [ -z "$6" ]
then
    LDFLAGS=""
else
    LDFLAGS="$6"
fi

OUTDIR="$7"

verilator -CFLAGS "$CFLAGS" -LDFLAGS "$LDFLAGS" --no-timing --top-module $TOP_MODULE -Wall -cc --trace-fst --exe -x-assign 0 --build --prefix Vmodel \
--Mdir $OUTDIR $VLT_FILES $VLT_CPP_FILES $VERILATOR_SCRIPT
