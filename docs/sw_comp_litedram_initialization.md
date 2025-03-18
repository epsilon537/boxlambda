---
hide:
  - toc
---

## LiteDRAM Initialization

When the *litedram_gen.py* script generates the LiteDRAM Verilog core (based on the given *.yml* configuration file), it also generates the core's CSR register accessors for software:

- For FPGA: [https://github.com/epsilon537/boxlambda/tree/master/gw/components/litedram/arty/sw/include/generated](https://github.com/epsilon537/boxlambda/tree/master/gw/components/litedram/arty/sw/include/generated)
- For simulation: [https://github.com/epsilon537/boxlambda/tree/master/gw/components/litedram/sim/sw/include/generated](https://github.com/epsilon537/boxlambda/tree/master/gw/components/litedram/sim/sw/include/generated)

The most relevant files are **csr.h** and **sdram_phy.h**. They contain the register definitions and constants used by the memory initialization code. Unfortunately, these accessors are *not* the same for the FPGA and the simulated LiteDRAM cores. We're going to have to use separate software builds for FPGA and simulation.

### *Sdram_init()*

*Sdram_phy.h* also contains a function called *init_sequence()*. This function gets invoked as part of a more elaborate initialization function called *sdram_init()*. *Sdram_init()* is *not* part of the generated code, however. It's part of *sdram.c*, which is part of *liblitedram*, which is part of the base Litex repository, *not* the LiteDRAM repository:

[https://github.com/epsilon537/litex/tree/master/litex/soc/software/liblitedram](https://github.com/epsilon537/litex/tree/master/litex/soc/software/liblitedram)

![sdram_init()](assets/sdram_init.drawio.png)

*sdram_init() vs. init_sequence().*

It's not clear to me why the *liblitedram* is not part of the LiteDRAM repository but's not a big deal. I integrated the *sdram_init()* function from *liblitedram* in the BoxLambda code base and it's working fine.

To get things to build, I added Litex as a git submodule, to get access to *liblitedram*. I also tweaked some *CPPFLAGS* and include paths. The resulting CMakeList is checked in here:

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ddr_test/CMakeLists.txt](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ddr_test/CMakeLists.txt)

Note: *liblitedram* expects a standard C environment.

### The DDR Test Application

The DDR test program is located here:

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ddr_test/ddr_test.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/ddr_test/ddr_test.c)

The program boots from internal memory. It invokes *sdram_init()*, performs a memory test, and verifies CPU instruction execution from DDR by relocating a test function from internal memory to DDR and branching to it.

The memory test function used is a slightly modified version of the *memtest()* function provided by Litex in *liblitedram*.
