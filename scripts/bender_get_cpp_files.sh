#! /bin/bash

#This script extracts just the .c/.cpp files to be verilated from the nearest bender.yml manifest and writes
#those file paths in an output file.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file containing the cpp file list
OUTFILE="$2"

bender -d $SRC_DIR update

#Get all files from bender verilator target, filter out the cpp and c file, and put everything on one line,
bender -d $SRC_DIR script flist -t verilator | grep "\.cpp$\|\.c$" | tr '\n' ' ' > "$OUTFILE.tmp"

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No cpp filelist changes detected."
else
  echo "Updating cpp filelist".
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp
