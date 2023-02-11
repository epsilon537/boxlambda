#! /bin/bash

#This script prepares the code base for first use. Retrieving submodules, setting up build trees.
#This script is to be run from the directory it's located in, i.e. the boxlambda repository root directory).
#This script does not make any modifications outside of the boxlambda directory tree.

echo "Retrieving git submodules..."
git submodule update --init --recursive

echo "Creating build build trees..."
cmake --preset=sim
cmake --preset=arty-a7-35
cmake --preset=arty-a7-100

echo
echo "Setup complete."
