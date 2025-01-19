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

echo $PART
echo $CONSTRAINTS
echo $TOP_MODULE
echo $FREQ

ii=0

while [ $ii -le 10 ]
do
  ii=$(( $ii + 1 ))
  rm -f nextpnr.log && nextpnr-xilinx --randomize-seed --chipdb $CHIPDB_DIR/$PART.bin --xdc $CONSTRAINTS --json $TOP_MODULE.json --write $TOP_MODULE_routed.json --fasm $TOP_MODULE.fasm --freq $FREQ -l nextpnr.log
  if [ $? -eq 0 ]; then
    echo "Routing completed successfully."

    fasm2frames --part $PART-1 --db-root /nix/store/ibcpbn8yvbz42ljyhvmg4bdsrac3q4wc-nextpnr-xilinx-0.8.2/share/nextpnr/external/prjxray-db/artix7/ boxlambda_top.fasm boxlambda_top.frames

    xc7frames2bit --part_file /nix/store/ibcpbn8yvbz42ljyhvmg4bdsrac3q4wc-nextpnr-xilinx-0.8.2/share/nextpnr/external/prjxray-db/artix7/xc7a35tcsg324-1/part.yaml --part_name $PART-1 --frm_file boxlambda_top.frames --output_file boxlambda_top.bit
    break
  fi
  echo "Routing failed. Retrying..."
done

