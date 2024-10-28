#! /bin/bash

# This script is used by the build system. It extracts the Vivado IP .xci files
# from the nearest bender.yml manifest and generates a tcl subscript to be
# included in the main vivado_create_project.tcl script.

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <src dir> <outfile> <bl_target_fpga>"
  exit 1
fi

SRC_DIR="$1"

# $2 = output file, containing import_ip tcl script for vivado
OUTFILE="$2"

# $3 = BL_TARGET_FPGA
BL_TARGET_FPGA="$3"

bender -d $SRC_DIR update

vivado_ips=`bender -d $SRC_DIR script flist -t  vivado_ip_$BL_TARGET_FPGA`

touch $OUTFILE.tmp

for ip in $vivado_ips
do
    echo import_ip $ip >> $OUTFILE.tmp
done

if cmp --silent -- "$OUTFILE" "$OUTFILE.tmp"; then
  echo "No Vivado IP filelist changes detected."
else
  echo "Updating Vivado IP filelist".
  cp $OUTFILE.tmp $OUTFILE
fi

rm -f $OUTFILE.tmp

