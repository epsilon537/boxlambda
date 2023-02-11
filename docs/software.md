Software Components
===================
LiteDRAM Initialization
-----------------------
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

The program boots from internal memory. It invokes *sdram_init()*, then performs a memory test over user port 0, followed by user port 1. Finally, the program verifies CPU instruction execution from DDR by relocating a test function from internal memory to DDR and branching to it.

The memory test function used is a slightly modified version of the *memtest()* function provided by Litex in *liblitedram*.

Picolibc
--------
BoxLambda uses the Picolibc standard C library implementation.
[Picolibc](https://github.com/picolibc/picolibc) is a Newlib variant, blended with AVR libc, optimized for systems with limited memory.
[Newlib](https://www.sourceware.org/newlib/) is the de-facto standard C library implementation for embedded systems. 

### Building Picolibc
A BoxLambda Picolibc fork has been added as a git submodule to BoxLambda's repository: [sub/picolibc/](https://github.com/epsilon537/picolibc/tree/3cd5bea5ad034d574670a7a85b2221d26224b588).

#### Picolibc Configuration Scripts - RV32IMC
A Picolibc build for a new system requires configuration scripts for that system in the [picolibc/scripts/](https://github.com/epsilon537/picolibc/tree/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts) directory. The scripts are named after the selected processor configuration. They specify such things as the compiler toolchain to use, GCC processor architecture flags, and CPP preprocessor flags tweaking specific library features. 

I'm using RISCV ISA-string **rv32imc** as the base name for the new scripts I'm creating. This corresponds with the default **-march** value of BoxLambda's GCC toolchain:

```
riscv32-unknown-elf-gcc -Q --help=target
The following options are target specific:
  -mabi=                                ilp32
  -malign-data=                         xlen
  -march=                               rv32imc
  -mbranch-cost=N                       0
  -mcmodel=                             medlow
  -mcpu=PROCESSOR
  -mdiv                                 [disabled]
  -mexplicit-relocs                     [disabled]
  -mfdiv                                [disabled]
  -misa-spec=                           2.2
  -mplt                                 [enabled]
  -mpreferred-stack-boundary=           0
  -mrelax                               [enabled]
  -mriscv-attribute                     [enabled]
  -msave-restore                        [disabled]
  -mshorten-memrefs                     [enabled]
  -msmall-data-limit=N                  8
  -mstrict-align                        [disabled]
  -mtune=PROCESSOR

  Supported ABIs (for use with the -mabi= option):
    ilp32 ilp32d ilp32e ilp32f lp64 lp64d lp64f

  Known code models (for use with the -mcmodel= option):
    medany medlow

  Supported ISA specs (for use with the -misa-spec= option):
    2.2 20190608 20191213

  Known data alignment choices (for use with the -malign-data= option):
    natural xlen
```

The easiest way to create the new scripts is to derive them from existing scripts for similar platforms. I derived the *rv32imc* configuration files  from the existing *rv32imac* configuration files:

- [do-rv32imc-configure](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/do-rv32imc-configure) is based on [do-rv32imac-configure](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/do-rv32imac-configure).
- [cross-rv32imc_zicsr.txt](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/cross-rv32imc_zicsr.txt) is based on [cross-rv32imac_zicsr.txt](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/cross-rv32imac_zicsr.txt).
- [run-rv32imc](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/run-rv32imc) is based on [run-rv32imac](https://github.com/epsilon537/picolibc/blob/3cd5bea5ad034d574670a7a85b2221d26224b588/scripts/run-rv32imac).

*Zicsr* stands for RISCV Control and Status Registers. These are always enabled on Ibex.

The differences between the derived scripts and the base scripts are minimal:

- They are referencing the *riscv32-unknown-elf* GCC toolchain used by BoxLambda.
- The *-march* flag is set to *rv32imc* (no 'a' - atomic instructions). 

#### picolibc_build.sh
![Building Picolibc.](assets/building_picolibc.drawio.png)

With the configuration scripts in place, we can build and install the picolibc library. We have to supply a build directory and an install directory.
I put the build directory in **boxlambda/sw/picolibc-build** and the install directory in **boxlambda/sw/picolibc-install**.

I grouped the picolibc build and install instructions in a [picolibc_build.sh](https://github.com/epsilon537/boxlambda/blob/master/scripts/picolibc_build.sh) shell script. The picolibc install directory is checked in so there's no need to run the *picolibc_build.sh* as part of the repository setup. The script is only needed in case we want to regenerate the picolibc library (e.g. after updating to a newer version of the library).

### Bootstrap - Some Glue Required

![Picolibc on BoxLambda.](assets/picolibc_on_boxlambda.drawio.png)

*Picolibc on BoxLambda.*

Picolibc is a relatively generic code base that needs to be tied to the platform it's running on to function properly. To bring up the library on BoxLambda, we need to supply three pieces of code:
- A Vector Table
- A Link Map
- Standard IO Setup

More detail for each of these follows in the subsections below. I have grouped them into a single software component called **bootstrap**:

[https://github.com/epsilon537/boxlambda/tree/master/sw/components/bootstrap](https://github.com/epsilon537/boxlambda/tree/master/sw/components/bootstrap)

An application wishing to use the standard C library has to link in this bootstrap component along with the picolibc library itself. 

#### The Vector Table

The vector table is a table with code entry points for all sorts of CPU events: interrupts, exceptions, etc. The Boot/Reset Vector, i.e. the very first instruction executed when the CPU comes out of reset, is part of this table.

I'm using the Vector Table from the *Hello World* example program included in the *ibex_wb* repository. The Vector Table file is located at [boxlambda/sw/components/bootstrap/vectors.S](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/vectors.S).

The Ibex Boot/Reset vector is at offset 0x80. After some CPU register initialization, the code branches off to **_start**, the entry point into picolibc's **crt0** module.

*Crt0*, C-Run-Time-0, is the Standard C library code in charge of setting up a C environment (zeroing the BSS segment, setting up the stack, etc.) before calling **main()**.

#### Standard Input, Output, and Error

The picolibc integrator needs to supply *stdin*, *stdout*, and *stderr* instances and associated *getc()* and *putc()* implementations to connect them to an actual IO device.
We'll be using the UART as our IO device for the time being. Down the road, we can extend that with keyboard input and screen output implementation.

```
static struct uart *uartp = 0;

static int uart_putc(char c, FILE *file) {
  int res;
  
  (void) file;		/* Not used in this function */

  if (!uartp) {
    res = EOF;
  }
  else {
    while (!uart_tx_ready(uartp));
    uart_tx(uartp, (uint8_t)c);
    res = (int)c;
  }
  
  return res;
}

static int uart_getc(FILE *file) {
  int c;
  (void) file;		/* Not used in this function */

  if (!uartp) {
    c = EOF;
  }
  else {
    while (!uart_rx_ready(uartp));
    c = (int)uart_rx(uartp);
  }
  
  return c;
}

static FILE __stdio = FDEV_SETUP_STREAM(uart_putc,
					uart_getc,
					NULL,
					_FDEV_SETUP_RW);


FILE *const stdin = &__stdio;
FILE *const stdout = &__stdio;
FILE *const stderr = &__stdio;

void set_stdio_to_uart(struct uart *uart) {
  uartp = uart;
}
```

[boxlambda/sw/components/bootstrap/stdio_to_uart.c](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/stdio_to_uart.c)

The **set_stdio_to_uart()** function is to be called from the application, before any standard library calls that require standard IO. The application needs to provide a pointer to an initialized *uart* object.

#### The Link Map

We have to tell the linker where in memory to place the program code, data, and stack.

I'm using the Link Map provided by picolibc, slightly modified to include the vector table.

The picolibc link map expects the user to define the following symbols:

- **__flash** and **__flash_size**: The location and size of the read-only section of the image, containing code and read-only data,
- **__ram** and **__ram_size**: The location and size of the read-write section of the image, containing data segments, bss, and stack.
- **__stack_size**: The stack size.

I created a link map file for BoxLambda's internal memory since that's all we've got for the time being. I dedicated the first half (32KB) to the read-only section and the 2nd half (32KB) to the read-write section:

```
__flash = 0x00000000; /*'flash' is the read-only section of the image, containing code and read-only data*/
__flash_size = 32k;
__ram = 0x00008000;   /*'ram' is the read-write section of the image, containing data segments, bss and stack*/
__ram_size = 32k;
__stack_size = 512;
```

[boxlambda/sw/components/bootstrap/link_internal_mem.ld](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/link_internal_mem.ld)

I can't say that I like this link map. There's no good reason to split internal memory in two this way, I don't like the symbol names being used, and I don't understand half of what's going on in this very big and complicated link map file. Now is not the time to design a new link map for BoxLambda though. We don't even have external memory defined yet. To be revisited.

### Linking against the Picolibc library: The Picolibc GCC specs file

To link the picolibc library into an application image, the picolibc *spec file* needs to be passed to GCC. The Picolibc GCC specs file expects absolute paths, however. I'm using CMake's *configure_file()* to replace placeholders 
in [scripts/picolibc.specs.in](https://github.com/epsilon537/boxlambda/blob/master/scripts/picolibc.specs.in) with the project source directory's absolute path. The resulting *picolibc.specs* is written in the root of the build tree. This way, the Picolibc library build for BoxLambda can be checked into the source tree and the user won't need to build and install it from source when setting up BoxLambda.

The code snippet below is taken from the SW [CMakeList](https://github.com/epsilon537/boxlambda/blob/master/sw/CMakeLists.txt):

```
#The GCC specs file expects absolute paths. I'm using configure_file() to replace placeholders 
#in picolibc.specs.in with PROJECT_SOURCE_DIR. The resulting picolibc.specs is written in the root of the build tree.
configure_file(${PROJECT_SOURCE_DIR}/scripts/picolibc.specs.in picolibc.specs @ONLY)

#Set the generated specs files as standard compile and link options.
set(SPECS "--specs=${CMAKE_CURRENT_BINARY_DIR}/picolibc.specs")
add_compile_options(${SPECS})
add_link_options(${SPECS} "LINKER:--gc-sections")
```

### The Picolibc Test Application

The test application program running on the Ibex processor is located in [sw/projects/picolibc_test/picolibc_test.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/picolibc_test/picolibc_test.c)

```
#include <stdio.h>
#include <string.h>
#include "stdio_to_uart.h"
#include "uart.h"
#include "platform.h"

static struct uart uart0;

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
}

int main(void) {
  int v = 123;
  static char m[10] = {0};
  char c;

  //Some basic libc tests:

  memset(m, '!', sizeof(m)-1);
  
  printf("printf in main() v=%d, m=%s.\n", v, m);

  printf("Enter character: ");
  c = getc(stdin);
  printf("Character entered: ");
  putc(c, stdout);
   
  return 0;
}
```


Notice the **_init()** function. This function is executed by the picolibc startup code before calling **main()**. This is where we set up the UART and stdio.

### Footprint

A quick examination of the test program *picolibc_test.elf* file shows:

- a .text (code) segment size of 0x2a38 = 10.5Kbytes
- a .data (initialized data) segment size of 0x28 = 40 bytes
- a .bss (zero-initialized data) segment size of 0x18 = 24 bytes
- a .stack size of 0x200 = 512 bytes

This all fits comfortably within our 64KB internal memory.
