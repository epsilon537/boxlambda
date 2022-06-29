TOP_DIR := ../..
VIVADO_TCL := $(TOP_DIR)/build_sys/vivado.tcl
PRG_BITSTREAM_TCL := $(TOP_DIR)/build_sys/prg_bitstream.tcl
PART = xc7a35ticsg324-1L
BENDER_GEN_SCRIPT = generated/sources.tcl
MEM_FILES := $(shell bender script flist -t memory)
CONSTRAINTS := $(shell bender script flist -n -t constraints)
CONSTRAINTS_GEN_SCRIPT = generated/constraints.tcl
MEM_FILES_GEN_SCRIPT = generated/mem_files.tcl
IP_ADDRESS := $(shell grep nameserver /etc/resolv.conf | sed 's/nameserver //')

#Flags
OOC = 1

.PHONY: always
always:
	bender update

.PHONY: constraints
constraints: $(CONSTRAINTS)
	$(RM) -f $(CONSTRAINTS_GEN_SCRIPT)
	$(foreach constraint, $^, echo add_files -fileset constrs_1 $(constraint) >> $(CONSTRAINTS_GEN_SCRIPT))
ifdef OOC
	$(foreach constraint, $^, echo set_property USED_IN {synthesis out_of_context} [get_files $(constraint)] >> $(CONSTRAINTS_GEN_SCRIPT))
endif

.PHONY: mem_files
mem_files: 
	$(RM) -f $(MEM_FILES_GEN_SCRIPT)
	$(foreach mem_file, $(MEM_FILES), echo add_files -norecurse $(mem_file) >> $(MEM_FILES_GEN_SCRIPT))

%.mem :
	$(MAKE) -C $(@D)

$(BENDER_GEN_SCRIPT): always
	mkdir -p generated
	bender script -t synthesis vivado > $(BENDER_GEN_SCRIPT)

.PHONY: synth dryrun
dryrun synth impl: $(BENDER_GEN_SCRIPT) mem_files constraints
	vivado -nolog -nojournal -mode batch -source $(VIVADO_TCL) \
	-tclargs -project generated/project -cmd $@ -part $(PART) \
	-sources $(BENDER_GEN_SCRIPT) -constraints $(CONSTRAINTS_GEN_SCRIPT) \
	-mem_files $(MEM_FILES_GEN_SCRIPT) -outputDir generated

.PHONY: run
run:
	vivado -nolog -nojournal -mode batch -source $(PRG_BITSTREAM_TCL) -tclargs -bitstream generated/project.runs/impl_1/*.bit -ip $(IP_ADDRESS)

.PHONY: clean
clean:
	$(RM) *.log
	$(RM) -rf generated
