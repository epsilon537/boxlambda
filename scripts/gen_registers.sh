#! /bin/bash

# This script is used by the build system. It generates the register access
# layer for the given list of module using Corsair.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <module_list>"
  exit 1
fi

for MODULE in $*
do
  python3 -m corsair -c definitions/$MODULE.config -r definitions/$MODULE.yaml;
done
