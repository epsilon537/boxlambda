# Makefile

# defaults
SIM ?= icarus
TOPLEVEL_LANG ?= verilog

WAVES=1

#EXTRA_ARGS += --trace --trace-structs
#EXTRA_ARGS += --trace-fst --trace-structs

VERILOG_SOURCES += ../rtl/av2wb.sv
# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = av2wb

# MODULE is the basename of the Python test file
MODULE = av2wb_test

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim