#! /bin/bash

#picolibc build script. You don't need to run this. The picolibc library for boxlambda is checked-in at sw/picolibc-install/.
#This script is only needed in case we want to regenerate the picolibc library (e.g. after updating to a newer version of the library).

if [[ "$#" < 3  || "$1" == "-h" ]]
then
  echo "$0 <picolibc repo dir> <picolibc build dir> <picolibc install dir>"
  exit 1
fi

#This is where the picolibc repository lives
PICOLIBC_SUB_DIR="$1"
#This directory is used to build picolibc for our target.
PICOLIBC_BUILD_DIR="$2"
#This is where picolibc is installed after it has been built.
PICOLIBC_INSTALL_DIR="$3"

echo "Creating picolibc build and install dirs..."
rm -rf $PICOLIBC_BUILD_DIR
rm -rf $PICOLIBC_INSTALL_DIR
mkdir -p $PICOLIBC_BUILD_DIR
pushd .

echo "Executing picolibc configure script..."
cd $PICOLIBC_BUILD_DIR
$PICOLIBC_SUB_DIR/scripts/do-boxlambda-configure -Dprefix=$PICOLIBC_INSTALL_DIR -Dspecsdir=$PICOLIBC_INSTALL_DIR

echo "Building picolibc..."
ninja
ninja install
popd

