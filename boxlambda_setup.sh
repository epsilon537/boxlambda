#! /bin/bash

#This script prepares the code base for first use. Retrieving submodules, setting up build trees.
#This script is to be run from the directory it's located in, i.e. the boxlambda repository root directory).
#This script does not make any modifications outside of the boxlambda directory tree.


if [[ "$#" > 0 && "$1" == "-h" ]]
then
  echo "$0 [-h] [-s]"
  echo "-h: Show help."
  echo "-s: Check out all submodules to boxlambda branch head (instead of detached head)."
  exit 1
fi

echo "Retrieving git submodules..."
git submodule update --init --recursive


if [[ "$#" > 0 && "$1" == "-s" ]]
then
    echo "Recursively checking out submodules to HEAD of boxlambda branch."
    git submodule foreach --recursive git pull
    git submodule foreach --recursive git checkout boxlambda
fi

echo "Creating build build trees..."
cmake --preset=sim-a7-35
cmake --preset=sim-a7-100
cmake --preset=arty-a7-35
cmake --preset=arty-a7-100

echo
echo "Setup complete."
