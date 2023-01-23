#! /bin/bash

#This script prepares the code base for first use. Retrieving submodules, setting up build trees.
#This script is to be run from the directory it's located in, i.e. the boxlambda repository root directory).
#This script does not make any modifications outside of the boxlambda directory tree.

echo "Retrieving git submodules..."
git submodule update --init --recursive

echo "Creating build/fpga build tree..."
mkdir -p build/fpga
cmake -DCMAKE_TOOLCHAIN_FILE=scripts/toolchain.cmake -DCMAKE_BUILD_TYPE=fpga -S . -B build/fpga

echo "Creating build/sim build tree..."
mkdir -p build/sim
cmake -DCMAKE_TOOLCHAIN_FILE=scripts/toolchain.cmake -DCMAKE_BUILD_TYPE=sim -S . -B build/sim

echo
echo "Setup complete. In directory build/fpga or build/sim, type 'make help' to see available build target for FPGA resp. simulation."
