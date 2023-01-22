#! /bin/bash

if [[ "$#" == 0  || "$1" == "-h" ]]
then
  echo "$0 <input mem file> <output mem file> <script file>"
  exit 1
fi

IN_MEM_FILE=$1
OUT_MEM_FILE=$2
SCRIPT_FILE_NAME=$3

cp $IN_MEM_FILE $OUT_MEM_FILE
echo "add_files -norecurse $OUT_MEM_FILE" > $SCRIPT_FILE_NAME
