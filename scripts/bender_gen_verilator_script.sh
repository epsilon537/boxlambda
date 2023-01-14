#! /bin/sh

# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file, containing verilator script
OUTFILE="$2"

# $3 = optional OOC target
#-z tests for empty string.
if [ -z "$3" ]
then
  MIN_T_OOC=""
else
  MIN_T_OOC="-t$3"
fi

bender -d $SRC_DIR update

bender -d $SRC_DIR script $MIN_T_OOC verilator | tr '\n' ' ' > $OUTFILE
