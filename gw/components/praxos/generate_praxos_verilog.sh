#! /bin/bash

#Invoke yoysys script to convert Praxos VHDL source to Verilog (rtl/praxos_generated.v)
#The generated code is checked in. The verilog code need to be regenerated when the VHDL code in
#submodule sub/Praxos changes.

yosys -m ghdl -s praxos_vhdl_to_verilog.yosys
