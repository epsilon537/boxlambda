#! /bin/bash

#This script generates a yosys synth script and executes it.
#This script is used by the build system.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <flist_plus> <top>"
  exit 1
fi

FLIST_PLUS="$1"
TOP_MODULE="$2"

#Convert systemverilog to verilog and put all the files in a verilog subdirectory.
mkdir -p verilog
echo -n "sv2v --write=verilog --top=$TOP_MODULE -DOPENXC7 -DSYNTHESIS " > sv2v.in
cat $FLIST_PLUS | sed -e "s#+incdir+#--incdir=#" -e "s#+define+#-D#" | tr '\n' ' ' >> sv2v.in
source sv2v.in

#Generate and execute yosys synth script
echo "read_verilog -sv verilog/*.v; hierarchy; synth_xilinx -flatten -abc9 -arch xc7 -top $TOP_MODULE; write_json $TOP_MODULE.json;" > yosys.synth

yosys -s yosys.synth

