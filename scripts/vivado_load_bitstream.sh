#! /bin/bash

#This script is to be executed from a GW project build tree. It loads the generated bitstream file onto the
#connected FPGA target.
#This script is used by the build system.

#BASEDIR is the location of the current script, i.e. the scripts directory
BASEDIR=$(dirname "$0")

vivado -nolog -nojournal -mode batch -source $BASEDIR/prg_bitstream.tcl -tclargs -bitstream ./project.runs/impl_1/*.bit
