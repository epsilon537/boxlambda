COMPONENT_MAKEFILES = $(shell find components -name Makefile)
PROJECT_MAKEFILES = $(shell find projects -name Makefile)

.PHONY: clean
clean: bender_update
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) clean && ) echo "Done"

.PHONY: bender_update
bender_update: 
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) bender_update && ) echo "Done"
