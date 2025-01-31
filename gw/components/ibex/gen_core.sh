#! /bin/bash

#This script updates the gw/components/ibex/ibex_out files from sub/ibex.
#This should be run when the sub/ibex repo has been changed (e.g. synced to a newer
#version). Keep in mind that the Bender.yml file list may have to be updated as well.
#ibex_out/ibex-verilator/lowrisc_ibex_ibex_top_0.1.vc has a file list that can be
#used as a starting point for the Bender.yml manifest.

SCRIPT_DIR=`realpath $(dirname "$0")`

pushd .

cd ../../../sub/ibex

fusesoc --cores-root . run --target=lint --setup --build-root ../../gw/components/ibex/ibex_out lowrisc:ibex:ibex_top

popd
