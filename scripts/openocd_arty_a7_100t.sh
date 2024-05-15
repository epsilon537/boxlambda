#! /bin/bash

SCRIPT_DIR=`realpath $(dirname "$0")`
openocd -f $SCRIPT_DIR/arty_a7_100t.openocd.cfg

