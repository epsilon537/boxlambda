#! /bin/bash

# This script is used by the build system. It extracts the DFX constraints file from the nearest Bender.yml manifest 

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <constraint target> <toolchain target>"
  exit 1
fi

SRC_DIR="$1"

CONSTRAINT_TARGET="$2"
TOOLCHAIN_TARGET="$3"

CONSTRAINTS_FILE=`bender -d $SRC_DIR script flist -n -t $CONSTRAINT_TARGET -t $TOOLCHAIN_TARGET`

#Remove the generated Bender.lock file to keep the source tree clean.
rm -f $SRC_DIR/Bender.lock

#Output the constraints file name
echo $CONSTRAINTS_FILE

