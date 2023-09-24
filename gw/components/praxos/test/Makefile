# Makefile

# defaults
SIM ?= icarus
TOPLEVEL_LANG ?= verilog

WAVES=1

#Praxos wordcopy test
VERILOG_SOURCES += ../rtl/praxos_ctrl.sv ../rtl/av2wb.sv ../rtl/praxos_generated.v ../rtl/praxos_top.sv

#Praxos av2wb
#VERILOG_SOURCES += ../rtl/av2wb.sv

# use VHDL_SOURCES for VHDL files

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
#Praxos wordcopy test
TOPLEVEL = praxos_top

#Praxos av2wb
#TOPLEVEL = av2wb

# MODULE is the basename of the Python test file
#Praxos wordcopy test
MODULE = praxos_wordcopy_test

#Praxos av2wb test
#MODULE = av2wb_test

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim