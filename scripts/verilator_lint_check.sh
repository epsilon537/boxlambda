#! /bin/sh

TOP_MODULE="$1"
VLT_FILES_FILE="$2"
WAIVER_FILE="$3"
VERILATOR_SCRIPT="$4"

if [ -z "$5" ]
then
    CFLAGS=""
else
    CFLAGS="-CFLAGS $5"
fi

if [ -z "$6" ]
then
    LDFLAGS=""
else
    LDFLAGS="-LDFLAGS $6"
fi

verilator --lint-only $CFLAGS $LDFLAGS --top-module $TOP_MODULE --Wall `cat $VLT_FILES_FILE` --waiver-output $WAIVER_FILE `cat $VERILATOR_SCRIPT`

echo "Done. If no issues are found, verilator completes silently"
