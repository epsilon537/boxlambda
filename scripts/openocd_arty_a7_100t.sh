#! /bin/bash

if [[ "$#" == 0 || "$1" == "-h" ]]
then
  echo "$0 [-r]"
  echo "-r: Reset target before launching openocd"
  exit 1
fi

if [[ "$1" == "-r" ]]
then
  echo "Resetting target..."
  openFPGALoader -b arty_a7_100t -r
  sleep 3
  echo "Done."
fi

SCRIPT_DIR=`realpath $(dirname "$0")`
openocd -f $SCRIPT_DIR/arty_a7_100t.openocd.cfg

