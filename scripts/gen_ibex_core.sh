#! /bin/bash

#This script is invoked by the build system's 'make gen_ibex_core' rule.
#It exports the ibex core files from the given ibex submodule to the given export directory.
#It should be run when the ibex repo has been changed.

#Keep in mind that the Bender.yml file list mght have to be updated as well after running this script.
#In the export directory ibex-verilator/lowrisc_ibex_ibex_top_0.1.vc has a file list that can be
#used as a starting point for the Bender.yml manifest.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <ibex submodule dir> <ibex export dir>"
  exit 1
fi

REPO_DIR="$1"
EXP_DIR="$2"

mkdir -p $EXP_DIR

pushd .

cd $REPO_DIR

fusesoc --cores-root . run --target=lint --setup --build-root $EXP_DIR lowrisc:ibex:ibex_top

popd

