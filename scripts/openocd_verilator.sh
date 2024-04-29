#! /bin/bash

SCRIPT_DIR=`realpath $(dirname "$0")`
openocd -f $SCRIPT_DIR/verilator.openocd.cfg

