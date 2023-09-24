#! /bin/bash

#This script performs a lint check using verilator.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <top module> <.vlts file> <waiver file> <verilator script>"
  exit 1
fi

TOP_MODULE="$1"
VLT_FILES_FILE="$2"
WAIVER_FILE="$3"
VERILATOR_SCRIPT="$4"

if [ -z "$5" ]
then
    CFLAGS=""
else
    CFLAGS="-CFLAGS $5"
fi

echo "If no issues are found, verilator will complete silently."

verilator --lint-only $CFLAGS --no-timing --top-module $TOP_MODULE --Wall `cat $VLT_FILES_FILE` --waiver-output $WAIVER_FILE `cat $VERILATOR_SCRIPT`
