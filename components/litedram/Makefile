#Path to the root of the repository
TOP_DIR := ../..
TOP_MODULE := litedram_wrapper

#Flag indicating Out-of-Context Synthesis.
#The value corresponds to the Bender.yml target used to select OOC specific files and defines.
OOC = litedram

#Include common Make flags and rules
include $(TOP_DIR)/build_sys/common.mk

gen_sim_core:
	litedram_gen artya7dram.yml --sim --gateware-dir sim/rtl --software-dir sim/sw --name litedram

gen_arty_core:
	litedram_gen artya7dram.yml --gateware-dir arty/rtl --software-dir arty/sw --name litedram
