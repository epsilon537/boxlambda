# Makefile

# defaults
SIM ?= icarus
TOPLEVEL_LANG ?= verilog

WAVES=1

#EXTRA_ARGS += --trace --trace-structs
#EXTRA_ARGS += --trace-fst --trace-structs
VERILOG_SOURCES += @CMAKE_CURRENT_LIST_DIR@/rtl/reset_ctrl.sv @CMAKE_CURRENT_LIST_DIR@/rtl/button_conditioner.sv
# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = reset_ctrl

# MODULE is the basename of the Python test file
MODULE = reset_ctrl_test

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim