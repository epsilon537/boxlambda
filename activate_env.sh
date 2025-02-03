#! /bin/bash

#!/usr/bin/env bash
if [ "${BASH_SOURCE-}" = "$0" ]; then
    echo "You must source this script: \$ source $0" >&2
    exit 1
fi

echo "Note: This script should be sourced from a boxlambda workspace root directory. Different boxlambda workspaces may share the same environment."

pushd . > /dev/null
mkdir -p tools
cd tools

if [ -d oss-cad-suite ]; then
  echo "oss-cad-suite found."
else
  echo "Downloading and unpacking oss-cad-suite..."
  wget https://github.com/YosysHQ/oss-cad-suite-build/releases/download/2025-02-01/oss-cad-suite-linux-x64-20250201.tgz

  if tar xf oss-cad-suite-linux-x64-20250201.tgz ; then
    echo "OK"
  else
    echo "Unpack of oss-cad-suite failed. Aborting..."
    popd
    return 1
  fi
fi

export LOWRISC_TOOLCHAIN_VERSION=20240923
if [ -d ./lowrisc-toolchain-gcc-rv32imcb-$LOWRISC_TOOLCHAIN_VERSION-1 ]; then
  echo "lowrisc-toolchain found."
else
  echo "Downloading and installing lowrisc-toolchain..."
  wget https://github.com/lowRISC/lowrisc-toolchains/releases/download/$LOWRISC_TOOLCHAIN_VERSION-1/lowrisc-toolchain-gcc-rv32imcb-$LOWRISC_TOOLCHAIN_VERSION-1.tar.xz
  if tar xf lowrisc-toolchain-gcc-rv32imcb-$LOWRISC_TOOLCHAIN_VERSION-1.tar.xz ; then
    echo "OK"
  else
    echo "Unpack of lowrisc-toolchain failed. Aborting..."
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

echo "Activate OSS CAD Suite Environment..."
_ORIG_PS1="${PS1}"

if source ./tools/oss-cad-suite/environment ; then
  echo "OK"
else
  echo "OSS CAD Suite activation failed. Aborting..."
  return 1
fi

if [ -n "${ZSH_VERSION-}" ] ; then
    autoload -U colors && colors
    PS1="%{$fg[magenta]%}(BoxLambda)%{$reset_color%}${_ORIG_PS1}"
else
    PS1="\[\033[1;35m\]\342\246\227BoxLambda\342\246\230\[\033[0m\]${_ORIG_PS1}"
fi
export PS1

if python3 -m pip -qq check python-requirements.txt ; then
  echo "Required Python packages found."
else
  echo "Installing required Python packages..."
  if python3 -m pip install -qq -U -r python-requirements.txt ; then
    echo "OK"
  else
    "Pip install failed. Aborting..."
    return 1
  fi
fi

echo "Updating PATH..."

export BOXLAMBDA_WORKSPACE=`pwd`
export PATH=$BOXLAMBDA_WORKSPACE/tools:$BOXLAMBDA_WORKSPACE/tools/lowrisc-toolchain-gcc-rv32imcb-$LOWRISC_TOOLCHAIN_VERSION-1/bin:$BOXLAMBDA_WORKSPACE/scripts:$PATH

echo "Done. Enter 'deactivate' to deactivate the environment."

