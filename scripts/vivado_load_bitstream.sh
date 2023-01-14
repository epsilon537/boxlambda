#! /bin/sh

#BASEDIR is the location of the current script, i.e. the scripts directory
BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/prg_bitstream.tcl -tclargs -bitstream ./project.runs/impl_1/*.bit
