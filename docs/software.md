Software Components
===================
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

#### make setup
![Building Picolibc.](assets/building_picolibc.drawio.png)

With the configuration scripts in place, we can build and install the picolibc library. We have to supply a build directory and an install directory.
I put the build directory in **boxlambda/sw/picolibc-build** and the install directory in **boxlambda/sw/picolibc-install**.

I grouped the picolibc build and install instructions in a *setup* rule in the top-level Makefile:

```
PICOLIBC_SUB_DIR= $(abspath sub/picolibc) #This is where the picolibc repository lives
PICOLIBC_BUILD_DIR= sw/picolibc-build #This directory is used to build picolibc for our target.
PICOLIBC_INSTALL_DIR= $(abspath sw/picolibc-install) #This is where picolibc is installed after it has been built.

setup: submodule-setup
    mkdir -p $(PICOLIBC_BUILD_DIR)
    cd $(PICOLIBC_BUILD_DIR)
    $(PICOLIBC_SUB_DIR)/scripts/do-rv32imc-configure -Dprefix=$(PICOLIBC_INSTALL_DIR) -Dspecsdir=$(PICOLIBC_INSTALL_DIR)
    ninja
    ninja install
```

Ideally, I would just check in the picolibc install directory. However, that won't work because the generated files contain absolute paths. This means that a **make setup** step is necessary to set up the BoxLambda repository. Besides building and installing picolibc, this step will also set up the git submodules used by BoxLambda.
This also means that, before *make setup* is run, the *boxlambda/sw/picolibc-build* and *boxlambda/sw/picolibc-install* directories won't even exist. They are not part of the git repository.

Note that *make setup* does not make any modifications outside of the BoxLambda directory tree.

### Bootstrap - Some Glue Required

![Picolibc on BoxLambda.](assets/picolibc_on_boxlambda.drawio.png)

*Picolibc on BoxLambda.*

Picolibc is a relatively generic code base that needs to be tied to the platform it's running on to function properly. To bring up the library on BoxLambda, we need to supply three pieces of code:
- A Vector Table
- A Link Map
- Standard IO Setup

More detail for each of these follows in the subsections below. I have grouped them into a single software component called **bootstrap**:

[https://github.com/epsilon537/boxlambda/tree/develop/sw/bootstrap](https://github.com/epsilon537/boxlambda/tree/develop/sw/bootstrap)

An application wishing to use the standard C library has to link in this bootstrap component along with the picolibc library itself. 

#### The Vector Table

The vector table is a table with code entry points for all sorts of CPU events: interrupts, exceptions, etc. The Boot/Reset Vector, i.e. the very first instruction executed when the CPU comes out of reset, is part of this table.

I'm using the Vector Table from the *Hello World* example program included in the *ibex_wb* repository. The Vector Table file is located at [boxlambda/sw/bootstrap/vectors.S](https://github.com/epsilon537/boxlambda/blob/develop/sw/bootstrap/vectors.S).

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

[boxlambda/sw/bootstrap/stdio_to_uart.c](https://github.com/epsilon537/boxlambda/blob/develop/sw/bootstrap/stdio_to_uart.c)

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

[boxlambda/sw/bootstrap/link_internal_mem.ld](https://github.com/epsilon537/boxlambda/blob/develop/sw/bootstrap/link_internal_mem.ld)

I can't say that I like this link map. There's no good reason to split internal memory in two this way, I don't like the symbol names being used, and I don't understand half of what's going on in this very big and complicated link map file. Now is not the time to design a new link map for BoxLambda though. We don't even have external memory defined yet. To be revisited.

### Linking against the picolibc library

To link the picolibc library into an application image, the picolibc *spec file* needs to be passed to GCC. The code snippet below is taken from the *picolibc_test* program's [Makefile](https://github.com/epsilon537/boxlambda/blob/develop/projects/picolibc_test/src/Makefile):

```
#Compile with picolibc specs to pull in picolibc library code.
CFLAGS = --specs=$(TOP_DIR)/sw/picolibc-install/picolibc.specs -Wall -g -O1
```

### The Test Application

The test application program running on the Ibex processor is located in [boxlambda/projects/picolibc_test/src/picolibc_test.c](https://github.com/epsilon537/boxlambda/blob/develop/projects/picolibc_test/src/picolibc_test.c)

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
