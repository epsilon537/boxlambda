#Path to the root of the repository
TOP_DIR := ../..
TOP_MODULE_SIM := sim_main
TOP_MODULE_SYNTH := ibex_soc

#Compile and link flag for Verilator testbench code.
VERILATOR_CPPFLAGS = -CFLAGS "-I$(abspath $(TOP_DIR))/sub/wbuart32/bench/cpp/ -I$(abspath $(TOP_DIR))/sub/riscv-dbg/tb/remote_bitbang -g"

#Include common Make flags and rules
include $(TOP_DIR)/build_sys/common.mk

PRG_BITSTREAM_TCL := $(TOP_DIR)/build_sys/prg_bitstream.tcl

#Run test script
.PHONY: test
test: sim
	cd generated && source ../test/test.sh
