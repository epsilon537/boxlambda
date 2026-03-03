#! /bin/bash

# This script creates: a FAT image file containing the contents of the given source dir.
# This script is used by the build system.

if [[ "$#" < 2  || "$1" == "-h" ]]
then
  echo "$0 <img_file> <src dir>"
  exit 1
fi

IMG_FILE="$1"

SRC_DIR="$2"

dd if=/dev/zero of=$IMG_FILE bs=1M count=1
mkfs.fat -S 512 $IMG_FILE
mcopy -i $IMG_FILE -s $SRC_DIR/* ::/


