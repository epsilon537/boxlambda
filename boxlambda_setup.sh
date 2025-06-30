#! /bin/bash

#This script does not make any file system modifications outside of the boxlambda directory tree.
echo "Setting up BoxLambda. Installing tools if needed. Initializing git submodules and creating build tree."
echo "Note: This script should be sourced from a boxlambda workspace root directory."

if [ "${BASH_SOURCE-}" = "$0" ]; then
    echo "You must source this script: \$ source $0" >&2
    exit 1
fi

if [[ "$#" > 0 && "$1" == "-h" ]]
then
  echo "$0 [-h] [-s]"
  echo "-h: Show help."
  echo "-s: Check out all submodules to boxlambda branch head (instead of detached head)."
  exit 1
fi

#Checking availability of key tools that user has to provide.
if which vivado ; then
  echo "Vivado found."
else
  echo "Vivado not found. Please install Vivado and add it to your path."
fi

if [ -z "$RISCV_PREFIX" ]; then
  export RISCV_PREFIX=riscv64-unknown-elf
fi

#Install RISCV compiler
pushd . > /dev/null
mkdir -p tools
cd tools

if [ -d riscv32-boxlambda-elf ]; then
  echo "riscv32 toolchain found."
else
  echo "Unpacking riscv32 toolchain..."

  if tar xf ../assets/riscv32-boxlambda-elf.tgz ; then
    echo "OK"
  else
    echo "Unpack of riscv32 toolchain failed. Aborting..."
    popd
    return 1
  fi
fi

#Download and install additional tools.

if [ -d oss-cad-suite ]; then
  echo "oss-cad-suite found."
else
  echo "Downloading and unpacking oss-cad-suite..."
  wget https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2025-02-26/oss-cad-suite-linux-x64-20250226.tgz

  if tar xf oss-cad-suite-linux-x64-20250226.tgz ; then
    echo "OK"
  else
    echo "Unpack of oss-cad-suite failed. Aborting..."
    popd
    return 1
  fi
fi

export BENDER_VERSION=0.28.1
if [ -f ./bender ]; then
  echo "Bender found."
else
  echo "Downloading and installing Bender..."
  wget https://github.com/pulp-platform/bender/releases/download/v$BENDER_VERSION/bender-$BENDER_VERSION-x86_64-linux-gnu.tar.gz
  if tar xf bender-$BENDER_VERSION-x86_64-linux-gnu.tar.gz ; then
    echo "OK"
  else
    echo "Unpack of Bender failed. Aborting..."
    popd
    return 1
  fi
fi

popd > /dev/null

#Activate the environment
source activate_env.sh

#Install required Python packages.
echo "Installing required Python packages..."
if python3 -m pip install -qq -U -r python-requirements.txt ; then
  echo "OK"
else
  "Pip install failed. Aborting..."
  return 1
fi
cp -f python-requirements.txt ./tools/oss-cad-suite/.python_packages_installed

echo "Retrieving git submodules..."
git submodule update --init --recursive

if [[ "$#" > 0 && "$1" == "-s" ]]
then
    echo "Recursively checking out submodules to HEAD of boxlambda branch."
    git submodule foreach --recursive git checkout boxlambda
    echo "Recursively pulling from remote."
    git submodule foreach --recursive git pull
fi

#Install LiteX.
#When litex_setup is run, it creates a bunch of new directories under sub/.
#sub/migen/ is one of them.
if [ -d ./sub/migen ]; then
  echo "Litex found."
else
  echo "Installing Litex..."
  pushd . > /dev/null
  cd sub/litex/
  if ./litex_setup.py --init --install ; then
    echo "OK"
  else
    "Litex install failed. Aborting..."
    return 1
  fi

  popd
fi

echo "Creating build build trees..."

rm -rf build

cmake --fresh --preset=sim-a7-100
cmake --fresh --preset=arty-a7-100

#Run the code generation rules
pushd .
cd build/sim-a7-100
make cgen
popd
pushd .
cd build/arty-a7-100
make cgen
popd

#Deactivate the environment
deactivate

echo
echo "Setup complete."
echo "Source activate_env.sh to activate the environment."

