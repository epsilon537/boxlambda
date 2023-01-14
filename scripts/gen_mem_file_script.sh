#! /bin/sh

INFILE=$1
OUTFILE=$2

INFILE_BASENAME=$(basename $INFILE)
OUTDIR=$(dirname $OUTFILE)

cp $INFILE $OUTDIR 
echo "add_files -norecurse $OUTDIR/$INFILE_BASENAME" > $OUTFILE
