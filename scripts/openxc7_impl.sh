#! /bin/bash

#This script generates a bitstream file using the openXC7 tools.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <part> <constraints.xdc> <top module name> <target frequency>"
  exit 1
fi

SCRIPT_DIR=`realpath $(dirname "$0")`
CHIPDB_DIR=$SCRIPT_DIR/../chipdb/

PART="$1"
CONSTRAINTS="$2"
TOP_MODULE="$3"
FREQ="$4"

nextpnr-xilinx --randomize-seed --router router2 --chipdb $CHIPDB_DIR/$PART.bin --xdc $CONSTRAINTS --json $TOP_MODULE.json --write ${TOP_MODULE}_routed.json --fasm $TOP_MODULE.fasm --freq $FREQ -l nextpnr.log

if [ $? -eq 0 ]; then
  echo "Routing completed successfully."

  fasm2frames --part $PART-1 --db-root $NEXTPNR_XILINX_DIR/share/nextpnr/external/prjxray-db/artix7/ boxlambda_top.fasm boxlambda_top.frames

  xc7frames2bit --part_file $NEXTPNR_XILINX_DIR/share/nextpnr/external/prjxray-db/artix7/$PART-1/part.yaml --part_name $PART-1 --frm_file boxlambda_top.frames --output_file boxlambda_top.bit
fi

