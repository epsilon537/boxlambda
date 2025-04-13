#!/bin/bash

if [ "${BASH_SOURCE-}" = "$0" ]; then
    echo "You must source this script: \$ source $0" >&2
    exit 1
fi

echo "Activating BoxLambda tools environments."
echo "Note: This script should be sourced from a boxlambda workspace root directory. Different boxlambda workspaces may share the same environment."

if [[ "$#" > 0 && "$1" == "-h" ]]
then
  echo "$0 [-h] [-r]"
  echo "-h: Show help."
  return 1
fi

if [ -d tools ]; then
  echo "Tools directory found."
else
  echo "Tools directory not found. Please source boxlambda_setup.sh to set up the workspace."
  return 1;
fi

if which vivado ; then
  echo "Vivado found."
else
  echo "Vivado not found. Please install Vivado and add it to your path."
fi

echo "Activate OSS CAD Suite Environment..."
_ORIG_PS1="${PS1}"

if source ./tools/oss-cad-suite/environment ; then
  echo "OK"
else
  echo "OSS CAD Suite activation failed. Aborting..."
  return 1
fi

#Tweaking the prompt
if [ -n "${ZSH_VERSION-}" ] ; then
    autoload -U colors && colors
    PS1="%{$fg[magenta]%}(BoxLambda `git rev-parse --abbrev-ref HEAD`)%{$reset_color%}${_ORIG_PS1}"
else
    PS1="(\e[35mBoxLambda `git rev-parse --abbrev-ref HEAD`\e[0m)${_ORIG_PS1}"
fi
export PS1

echo "Updating PATH..."

export BOXLAMBDA_WORKSPACE=`pwd`
export PATH=$BOXLAMBDA_WORKSPACE/tools:$BOXLAMBDA_WORKSPACE/tools/riscv32-boxlambda-elf/bin:$BOXLAMBDA_WORKSPACE/scripts:$PATH

echo "Done. Enter 'deactivate' to deactivate the environment."

