#! /bin/bash

#This script is invoked by the build system's 'make gen_litedram_core' rule and at build tree configuration time.
#It generates the litedram core files in the given export directory.
#It should be run when the litedram or litex repos have been changed.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <litedram yml file> <litedram export dir> [--sim]"
  exit 1
fi

YML_FILE="$1"
EXP_DIR="$2"
SIM_OPT="$3"

echo "gen_litedram_core flags:"
echo $1 $2 $3

rm -rf $EXP_DIR
mkdir -p $EXP_DIR

pushd .

cd $EXP_DIR
litedram_gen $YML_FILE $SIM_OPT --doc --gen_user_clkx2 --gateware-dir rtl --software-dir sw --name litedram

rm -rf ../../../../docs/registers/litedram-generated/*
sphinx-build -M markdown build/doc/ litedram-generated/
cp -f ../../../../scripts/litedram-index-orig.md litedram-generated/markdown/litedram-index.md
cp -rf litedram-generated/markdown/* ../../../../docs/registers/

popd

