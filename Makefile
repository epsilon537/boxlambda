#The top-level Makefile recursively runs the given target on component and project directories.

COMPONENT_MAKEFILES = $(shell find components -name Makefile)
PROJECT_MAKEFILES = $(shell find projects -name Makefile)
#FIXME: temporarily exclude riscv-dbg from the list because not ready to make changes in there.
#Will take care of this when we bring up riscv-dbg.
SUB_MAKEFILES = $(shell find sub -not -path "sub/ibex_wb/riscv-dbg/*" -name Makefile)

.PHONY: test
test:
	$(foreach makefile, $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) test && ) echo "Done"

.PHONY: lint
lint:
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) lint && ) echo "Done"

.PHONY: synth
synth:
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) synth && ) echo "Done"

.PHONY: impl
impl:
	$(foreach makefile, $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) synth && ) echo "Done"

.PHONY: clean
clean: bender_update
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES) $(SUB_MAKEFILES), \
	$(MAKE) -C $(dir $(makefile)) -q clean; if test $$? -le 1; then $(MAKE) -C $(dir $(makefile)) clean; fi;)

.PHONY: bender_update
bender_update: 
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) bender_update && ) echo "Done"
