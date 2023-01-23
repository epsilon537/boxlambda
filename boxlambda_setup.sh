#! /bin/bash

echo "Retrieving git submodules..."
git submodule update --init --recursive

echo "Creating build/fpga build tree..."
mkdir -p build/fpga
cmake -DCMAKE_TOOLCHAIN_FILE=scripts/toolchain.cmake -DCMAKE_BUILD_TYPE=fpga -S . -B build/fpga

echo "Creating build/sim build tree..."
mkdir -p build/sim
cmake -DCMAKE_TOOLCHAIN_FILE=scripts/toolchain.cmake -DCMAKE_BUILD_TYPE=sim -S . -B build/sim

echo "Setup complete. In build/fpga or build/sim, type 'make help' to see available build target for FPGA resp. simulation."
