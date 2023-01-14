#Path to the root of the repository
TOP_DIR := ../..
TOP_MODULE_SIM := sim_main
TOP_MODULE_SYNTH := ibex_soc

#Compile and link flag for Verilator testbench code.
VERILATOR_CPPFLAGS = -CFLAGS "-I$(abspath $(TOP_DIR))/sub/wbuart32/bench/cpp/ -I$(abspath $(TOP_DIR))/sub/riscv-dbg/tb/remote_bitbang -g"
VERILATOR_LDFLAGS = -LDFLAGS "-lncurses"

#Include common Make flags and rules
include $(TOP_DIR)/build_sys/common.mk

#Run verilator testbench for project
.PHONY: test
test: sim
	cd generated && ./Vmodel && cd ..
