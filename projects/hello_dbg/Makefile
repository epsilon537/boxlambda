#Path to the root of the repository
TOP_DIR := ../..

#Compile and link flag for Verilator testbench code.
VERILATOR_CPPFLAGS = --top-module sim_main -CFLAGS "-I$(abspath $(TOP_DIR))/sub/wbuart32/bench/cpp/ -I$(abspath $(TOP_DIR))/sub/riscv-dbg/tb/remote_bitbang -g"
VERILATOR_LDFLAGS = -LDFLAGS "-lncurses"

#Include common Make flags and rules
include $(TOP_DIR)/build_sys/common.mk

PRG_BITSTREAM_TCL := $(TOP_DIR)/build_sys/prg_bitstream.tcl
IP_ADDRESS := $(shell grep nameserver /etc/resolv.conf | sed 's/nameserver //')

#A rule to download the generated bitstream to target. There are many different ways to connect to a target. Below are the instructions for my
#setup. Customize as needed for your own setup.
.PHONY: run
run:
	vivado -nolog -nojournal -mode batch -source $(PRG_BITSTREAM_TCL) -tclargs -bitstream generated/project.runs/impl_1/*.bit -ip $(IP_ADDRESS)

#Run test script
.PHONY: test
test: sim
	cd generated && source ../sim/test.sh
