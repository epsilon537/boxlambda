#! /bin/sh

# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file, containing constraints tcl script for vivado
OUTFILE="$2"

rm -f $OUTFILE

bender -d $SRC_DIR update

CONSTRAINTS=`bender -d $SRC_DIR script flist -n -t constraints`

for constraint in $CONSTRAINTS
do
    echo add_files -fileset constrs_1 $constraint >> $OUTFILE
done
