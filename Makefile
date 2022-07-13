#The top-level Makefile is currently only used to recursively clean the components and projects directories and to recursively run bender update across components and projects.

COMPONENT_MAKEFILES = $(shell find components -name Makefile)
PROJECT_MAKEFILES = $(shell find projects -name Makefile)

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
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) clean && ) echo "Done"

.PHONY: bender_update
bender_update: 
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) bender_update && ) echo "Done"
