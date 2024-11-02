#! /bin/bash

# This script is used by the build system. It extracts the DFX constraints file from the nearest Bender.yml manifest 

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir>"
  exit 1
fi

# $1 = source directory
SRC_DIR="$1"
DFX_CONSTRAINTS=`bender -d $SRC_DIR script flist -n -t dfx_constraints`

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

#Output the DFX constraints file name
echo $DFX_CONSTRAINTS

