#Path to the root of the repository
TOP_DIR := ../..
INCLUDES = -I$(TOP_DIR)/sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc
SRCS = vectors.S stdio_to_uart.c ../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/uart.c

TARGET_CC_PREFIX = riscv32-unknown-elf
TARGET_CC := $(TARGET_CC_PREFIX)-gcc
TARGET_OBJCOPY := $(TARGET_CC_PREFIX)-objcopy
TARGET_OBJDUMP := $(TARGET_CC_PREFIX)-objdump

CFLAGS = --specs=../picolibc-install/picolibc.specs -Wall -g -O1

#nostartfiles: startfiles (crt0) will be linked in when producing a .elf
#relocatable: allows combining multiple .o's into a single .o.
LDFLAGS ?= -nostartfiles -Wl,-relocatable

OBJS := $(addsuffix .o,$(basename $(SRCS)))

#Compile everything into a single object file.
OUTFILES = bootstrap.lib.o
.PHONY: all
all: $(OUTFILES)

#Force rules are used throughout so all builds are full rebuilds.
#This is slow but always generates an up-to-date build.
#The current build system isn't smart enough for accurate dependency tracking.
#FIXME: Invest in a decent build system, with proper dependency tracking.

#Combine everything into one .o.
bootstrap.lib.o : $(OBJS) force
	$(TARGET_CC) $(LDFLAGS) -o $@ $(OBJS)

.PHONY: clean
clean:
	$(RM) -f $(OBJS) $(DEPS)
	$(RM) -f $(OUTFILES) *.map

%.o: %.c force
	$(TARGET_CC) $(CFLAGS) -c $(INCLUDES) -o $@ $<

%.o: %.S force
	$(TARGET_CC) $(CFLAGS) -c $(INCLUDES) -o $@ $<

.PHONY: force
force:
