#! /bin/bash

#This script performs a lint check using verilator.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <top module> <.vlts file> <waiver file> <verilator script> <verilator flags>"
  exit 1
fi

TOP_MODULE="$1"
VLT_FILES_FILE="$2"
WAIVER_FILE="$3"
VERILATOR_SCRIPT="$4"

#Shift out previous args so we can capture remaining args into FLAGS.
shift 4
FLAGS="$*"

echo "If no issues are found, verilator will complete silently."

verilator --lint-only $FLAGS --no-timing --top-module $TOP_MODULE --Wall `cat $VLT_FILES_FILE` --waiver-output $WAIVER_FILE `cat $VERILATOR_SCRIPT`
