#Path to the root of the repository
TOP_DIR := ../..

VERILATOR_CPPFLAGS = --top-module riscv_dbg_wrapper #-CFLAGS "-std=gnu++11 -O2"
VERILATOR_LDFLAGS = -LDFLAGS "-lncurses"

#Flag indicating Out-of-Context Synthesis
OOC = riscv-dbg

#Include common Make flags and rules
include $(TOP_DIR)/build_sys/common.mk

