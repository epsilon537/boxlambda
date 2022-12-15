#Path to the root of the repository
TOP_DIR := ../../../..

PROGRAM = ddr_test
INCLUDES = -I$(TOP_DIR)/sw/bootstrap \
	-I$(TOP_DIR)/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc \
	-I$(TOP_DIR)/sub/litex/litex/soc/software/liblitedram \
	-I$(TOP_DIR)/sub/litex/litex/soc/software/include \
	-I$(TOP_DIR)/sub/litex/litex/soc/software \
	-I$(TOP_DIR)/sub/litex/litex/soc/software/libbase \
    -I$(TOP_DIR)/sub/litex/litex/soc/cores/cpu/ibex \
	-I$(TOP_DIR)/components/litedram/sim/sw/include

#-DSDRAM_TEST_DISABLE disables the memory test function inside sdram_init()
#This function gets called before ddrctrl_init_done is written, which doesn't work.
#We're invoking memory test separately, from main, after sdram_init has completed.
CPPFLAGS = -DCONFIG_CPU_NOP=\"nop\" \
           -DMAIN_RAM_BASE=0x40000000UL \
           -DMAIN_RAM_BASE2=0x50000000UL \
           -DCSR_BASE=0x10030000 \
           -DSDRAM_TEST_DISABLE \
           -DMEMTEST_SIZE=8192

#Contains bootstrap code such as vectors and code to glue libc to the platform
LIBS = $(TOP_DIR)/sw/bootstrap/bootstrap.lib.o
SRCS = ../$(PROGRAM).c \
    ../memtest.c \
	$(TOP_DIR)/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c \
	$(TOP_DIR)/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c \
	$(TOP_DIR)/sub/litex/litex/soc/software/liblitedram/sdram.c

TARGET_CC_PREFIX = riscv32-unknown-elf
TARGET_CC := $(TARGET_CC_PREFIX)-gcc
TARGET_OBJCOPY := $(TARGET_CC_PREFIX)-objcopy
TARGET_OBJDUMP := $(TARGET_CC_PREFIX)-objdump

LINKER_SCRIPT = $(TOP_DIR)/sw/bootstrap/link_internal_mem.ld
#Compile with picolibc specs to pull in picolibc library code.
CFLAGS = --specs=$(TOP_DIR)/sw/picolibc-install/picolibc.specs $(CPPFLAGS) -Wall -g -O1

LDFLAGS = $(CFLAGS) -T $(LINKER_SCRIPT) -Wl,--gc-sections -Wl,--Map,$(PROGRAM).map

OBJS := $(addsuffix .o,$(basename $(SRCS)))

OUTFILES = $(PROGRAM).elf spram.mem $(PROGRAM).bin $(PROGRAM).dis

HEX2VMEM = $(TOP_DIR)/sub/ibex_wb/scripts/hex2vmem.pl
SENDAPP = $(TOP_DIR)/sub/ibex_wb/scripts/sendapp.sh

#Force rules are used throughout so all builds are full rebuilds.
#This is slow but always generates an up-to-date build.
#The current build system isn't smart enough for accurate dependency tracking.
#FIXME: Invest in a decent build system, with proper dependency tracking.
all: $(OUTFILES) force

$(PROGRAM).elf: $(OBJS) $(LIBS) $(LINKER_SCRIPT) force
	$(TARGET_CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $@

%.lib.o: force
	$(MAKE) -C $(@D)

#Rename the generated .mem file to spram.mem, which is what the FPGA build expects.
spram.mem : $(PROGRAM).mem
	cp -f $< $@
	rm -f $<

%.dis: %.elf
	$(TARGET_OBJDUMP) -SD $^ > $@

%.mem: %.hex
	$(HEX2VMEM) $< > $@

%.hex: %.elf
	$(TARGET_OBJCOPY) -O verilog --interleave-width=4 --interleave=4 --byte=0 $< $@

%.bin: %.elf
	$(TARGET_OBJCOPY) -O binary $< $@

%.o: %.c force
	$(TARGET_CC) $(CFLAGS) -c $(INCLUDES) -o $@ $<

%.o: %.S force
	$(TARGET_CC) $(CFLAGS) -c $(INCLUDES) -o $@ $<

.PHONY: clean
clean:
	$(RM) -f $(OBJS) $(DEPS) *.hex
	$(RM) -f $(OUTFILES) $(PROGRAM).map

.PHONY: force
force:
