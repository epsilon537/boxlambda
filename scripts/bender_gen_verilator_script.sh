#! /bin/bash

#This script extracts the files to be verilated from the nearest bender.yml manifest and generates 
#a verilator script.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <boxlambda src root dir> <outfile> <depfile target> <optional OOC target>"
  exit 1
fi


# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file, containing verilator script
OUTFILE="$2"

# $3 = depfile target
DEPFILE_TGT="$3"

# $4 = optional OOC target
#-z tests for empty string.
if [ -z "$4" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$4"
fi

bender -d $SRC_DIR update

bender -d $SRC_DIR script $MIN_T_OOC verilator > "$OUTFILE.tmp.split"
sort "$OUTFILE.tmp.split" > "$OUTFILE.tmp.sorted"
cat "$OUTFILE.tmp.split" | tr '\n' ' ' > "$OUTFILE.tmp"

if cmp --silent -- "$OUTFILE.sorted" "$OUTFILE.tmp.sorted"; then
  echo "No verilator script changes detected."
else
  echo "Updating verilator script".
  cp $OUTFILE.tmp.sorted $OUTFILE.sorted
  cp $OUTFILE.tmp $OUTFILE
fi

rm $OUTFILE.tmp*

#If depfile target is given...
if [ -n "$DEPFILE_TGT" ]
then
  #Generate a depfile: Prepend each line with <target> :
  bender -d $SRC_DIR script $MIN_T_OOC -t verilator flist | sed "s#^#$DEPFILE_TGT \: #" > $OUTFILE.dep
fi