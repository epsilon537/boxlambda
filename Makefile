#The top-level Makefile recursively runs the given target on component and project directories.

COMPONENT_MAKEFILES = $(shell find components -name Makefile)
PROJECT_MAKEFILES = $(shell find projects -name Makefile ! -path */src/*) #Exclude src/Makefiles (SW builds)
#Don't recurse into Pulpino or riscv-dbg
SUB_MAKEFILES = $(shell find sub -not -path "sub/pulpino/*" -not -path "sub/riscv-dbg/*" -name Makefile)
SW_MAKEFILES = $(shell find . -path "*/src/Makefile" -o -path "./sw/*/Makefile")
#This is where the picolibc repository lives
PICOLIBC_SUB_DIR= $(abspath sub/picolibc)
#This directory is used to build picolibc for our target.
PICOLIBC_BUILD_DIR= sw/picolibc-build
#This is where picolibc is installed after it has been built.
PICOLIBC_INSTALL_DIR= $(abspath sw/picolibc-install)

#'make setup' sets up the project for first use
#- It sets up the git submodules used.
#- It builds the picolibc library for Boxlambda. This is required as part of setup because picolibc has an installation step (ninja install) and uses absolute paths.
.PHONY: setup
setup: submodule-setup
	rm -rf $(PICOLIBC_BUILD_DIR)
	rm -rf $(PICOLIBC_INSTALL_DIR)
	mkdir -p $(PICOLIBC_BUILD_DIR)
	cd $(PICOLIBC_BUILD_DIR) && \
	$(PICOLIBC_SUB_DIR)/scripts/do-rv32imc-configure -Dprefix=$(PICOLIBC_INSTALL_DIR) -Dspecsdir=$(PICOLIBC_INSTALL_DIR) && \
	ninja && \
	ninja install

.PHONY: submodule-setup
submodule-setup:
	git submodule update --init --recursive

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
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES) $(SUB_MAKEFILES) $(SW_MAKEFILES), \
	$(MAKE) -C $(dir $(makefile)) -q clean; if test $$? -le 1; then $(MAKE) -C $(dir $(makefile)) clean; fi;)

.PHONY: bender_update
bender_update: 
	$(foreach makefile, $(COMPONENT_MAKEFILES) $(PROJECT_MAKEFILES), $(MAKE) -C $(dir $(makefile)) bender_update && ) echo "Done"

.PHONY: tags
tags:
	ctags-universal -e -R .
