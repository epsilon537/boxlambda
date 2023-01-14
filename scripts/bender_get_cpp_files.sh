#! /bin/sh

# $1 = meson.current_source_dir()
SRC_DIR="$1"

# $2 = output file containing the cpp file list
OUTFILE="$2"

bender -d $SRC_DIR update

#Get all files from bender verilator target, filter out the cpp and c file, and put everything on one line,
bender -d $SRC_DIR script flist -t verilator | grep "\.cpp$\|\.c$" | tr '\n' ' ' > "$OUTFILE"
