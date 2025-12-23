---
layout: post
title: 'Bringing up the SD-Card Controller and File System.'
comments: true
---

*Updated 23 December 2025: Removed reference to 'On WSL' documentation.*

I spent a couple of days bringing up an SD-Card Controller and a file system. It was a straightforward exercise thanks to Dan Gisselquist's [SDSPI repository](https://github.com/ZipCPU/sdspi).

Recap
-----
This is a summary of the current state of affairs for BoxLambda. We have:
- An Ibex RISC-V core, Wishbone shared bus, timer, two GPIO ports, UART core, and internal memory.
- DDR3 external memory access through the Litex Memory Controller.
- OpenOCD-based Debug Access, both on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Test builds running on Arty-A7-35T, Arty-A7-100T, and Verilator.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- A Linux CMake and Bender-based RTL build system.
- Automated testing on Verilator.

The MicroSD Card Interface and SPI Mode
---------------------------------------
![Arty A7 with MicroSD PMOD in JD port.](../assets/arty_w_microsd_pmod.jpeg)

*SDSPI in the BoxLambda Architecture.*

On the Arty, I'll be using Digilent's [MicroSD PMOD](https://digilent.com/shop/pmod-microsd-microsd-card-slot/) plugged into the **JD** Connector.

The MicroSD card can operate in two modes: SD card mode and SPI mode. In SPI mode, a 1-bit SPI bus is used as the interface between the SD-Card Controller (Master) and the SD card (Slave). SPI mode is selected by pulling the Chip Select line low.
The SDSPI core currently only supports SPI mode.

The SPI bus speed is software-configurable through a clock divider setting in the SDSPI core. The minimum value of this divider is 4. Given BoxLambda's 50MHz system clock rate, this limits the bus speed to 12.5MHz.

Other than the SPI signals (*SCK*, *MISO*, *MOSI*, *CS*), the MicroSD card interface has two *DAT* data lines that we won't be using and a *CD* Card Detect signal, which appears to be active-low, even though the [MicroSD PMOD Reference Manual](https://digilent.com/reference/pmod/pmodmicrosd/reference-manual?redirect=1) didn't say so.

The SDSPI Repo
--------------

The [SDSPI repository](https://github.com/ZipCPU/sdspi) makes life easy for an integrator. The repository contains:
- A SPI-based SD-Card Controller core with a Wishbone interface.
- A C++ test bench with SDSPISIM co-simulator.
- Documentation.
- Low-level driver functions to hook into the [FatFs](http://elm-chan.org/fsw/ff/00index_e.html) file system implementation.

The SDSPI Core
==============
![SDSPI in the BoxLambda Architecture.](../assets/Arch_Diagram_SDSPI_focus.drawio.png)

*SDSPI in the BoxLambda Architecture.*

I created a Test SoC containing all of the previously enabled BoxLambda components as well as the SDSPI core. See the Block Diagram Above. The core is instantiated in the [sdspi_test_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/sdspi_test/rtl/sdspi_test_soc.sv) top-level module as follows:

```
sdspi #(.OPT_LITTLE_ENDIAN(1'b1)) sdspi_inst (
		.i_clk(sys_clk), .i_sd_reset(ndmreset | (~sys_rst_n)),
		// Wishbone interface
		.i_wb_cyc(wbs[SDSPI_S].cyc), .i_wb_stb(wbs[SDSPI_S].stb), .i_wb_we(wbs[SDSPI_S].we),
		.i_wb_addr(wbs[SDSPI_S].adr[3:2]),
		.i_wb_data(wbs[SDSPI_S].dat_m),
		.i_wb_sel(wbs[SDSPI_S].sel),
		.o_wb_stall(wbs[SDSPI_S].stall),
		.o_wb_ack(wbs[SDSPI_S].ack),
		.o_wb_data(wbs[SDSPI_S].dat_s),
		// SDCard interface
		.o_cs_n(sdspi_cs_n), .o_sck(sdspi_sck), .o_mosi(sdspi_mosi),
		.i_miso(sdspi_miso), .i_card_detect(~sdspi_card_detect_n),
		// Interrupt
		.o_int(),
		// .. and whether or not we can use the SPI port
		.i_bus_grant(1'b1),
		// And some wires for debugging it all
		.o_debug()
	);
```

The Wishbone signals plug straight into the test SoC's system bus. The SD Card interface signals go straight to the top-level ports.
I currently don't have interrupts hooked up.



![SDSPI Block Diagram.](../assets/sdspi_block_diagram.drawio.png)

*SDSPI Simplified Block Diagram.*

I've been studying the core's internals a bit and created the above, simplified block diagram. I won't be going into the details here, however. Dan Gisselquist did a great job documenting the core in the [spec](https://github.com/ZipCPU/sdspi/blob/master/doc/gpl-3.0.pdf) and the source code.

SDSPISIM
========
On the Verilator test bench, the MicroSD card PMOD is replaced with an SDSPISIM co-simulator. SDSPISIM was easy to plug into BoxLambda's test bench. The interface is similar to the UARTSIM co-simulator, already in use in the test bench, and also provided by Dan Gisselquist.

Here're the hooks to both co-simulators in the test bench's **tick()** function. The tick() function is the heart of the test bench advancing the simulation by one clock cycle:

```
  //Feed SDSPI co-sim
  top->sdspi_miso = (*sdspi)(top->sdspi_cs_n, top->sdspi_sck, top->sdspi_mosi);

  //Feed our model's uart_tx signal and baud rate to the UART co-simulator.
  //and feed the UART co-simulator output to our model
  top->uart_rx = (*uart)(top->uart_tx, top->rootp->sim_main__DOT__dut__DOT__wb_uart__DOT__wbuart__DOT__uart_setup);
```

For the complete test bench code, see [sim_main.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/sdspi_test/sim/sim_main.cpp) in the *sdspi_test* project.

SDSPISIM reads from and writes to an **sdcard.img** file. That file can be mounted in Linux, so you can FAT format it and put files on it for the simulated system to use, or vice versa. See the [FatFS Test on Verilator](#fatfs-test-on-verilator) section below for an example.

SDSPI Operation
===============
The SDSPI core's register interface, the initialization sequence, and the overall operation of the core are well-documented in the SDSPI core [spec](https://github.com/ZipCPU/sdspi/blob/master/doc/spec.pdf).

[Sdtest.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/sdspi_test/sdtest.c) demonstrates and tests the SDSPI core operation. This is a modified version of Dan's *sdtest.c* in the [Zbasic repo](https://github.com/ZipCPU/zbasic). The *Zbasic* repo integrates the SDSPI core and other peripherals developed by Dan into a [ZipCPU Platform](https://zipcpu.com/projects.html).

*Sdtest.c* runs on the RISCV processor that's part of the SDSPI Test SoC build outlined above.

See [instructions below](#sdspi-test-on-verilator) for building and running the SDSPI test project.

FatFs
-----
[FatFs](http://elm-chan.org/fsw/ff/00index_e.html) is a lightweight software library for small systems that implements FAT file system support. It's written in ANSI C89 and has no dependencies other than a minimal C environment. It'll compile out of the box in virtually any environment.

FatFs itself does not provide the device/media-specific _Storage Device_ Controls*. Those have to come from the device implementer. Conveniently, the SDSPI repo does provide these functions for FatFs. Three files are provided: [sdcard.c, sdcard.h, and diskio.c](https://github.com/ZipCPU/sdspi/tree/master/sw).

Note: These three files have a dependency on a **board.h** header file which is not part of the SDSPI repo. An example *board.h* can again be found in the [Zbasic repo](https://github.com/ZipCPU/zbasic/blob/master/sw/zlib/board.h). I checked the BoxLambda version into the [boxlambda branch of the BoxLambda fork of the SDSPI repo](https://github.com/epsilon537/sdspi/blob/boxlambda/sw/board.h).


![FatFs Media Access Interface.](../assets/FatFs_Media_Access_Interface.drawio.png)

*FatFs Media Access Interface.*

Endianness
==========
Bringing up the file system should just be a matter of compiling the FatFs sources together with the Storage Device Control sources provided by the SDSPI repo. Here, I hit a snag, however. The first call to FatFs, **f_mount()** returned a **FR_NO_FILESYSTEM** error. At first, I thought there was something wrong with the formatting of my *sdcard.img* file, but that wasn't the case. Luckily, the SDSPI code contains plenty of debug logic. Once the debug code was enabled, the problem was easy to see: When reading data from the SD card image, groups of four bytes were getting swapped around. In other words, there was an endianness issue.

There is some word-to-byte-array casting going on in *sdcard.c* which assumed a big-endian system. This is because SDSPI is part of the ZipCPU ecosystem, which is a big-endian platform. BoxLambda is using a little-endian RISCV CPU, however.
I mentioned the issue to Dan and he provided a fix by adding a parameter that allows you to instantiate the core as a little-endian core:

```
	sdspi #(.OPT_LITTLE_ENDIAN(1'b1)) sdspi_inst (
		...
```

At the time of writing the little-endian option is not available yet on the master branch, but you can already find it on the repo's *dev* branch.

There is a secondary issue here: the word-to-byte array casting in *sdcard.c* silently assumes that passed-in byte arrays are word aligned. My first thought was to modify the *sdcard.c* code so that it works with arbitrary buffer alignment, but then Dan pointed out that word alignment would still be required if a DMA engine is used for data transfer to/from the SDSPI core. It's better to just accept that *sdcard.c* requires word alignment of passed-in buffers, i.e. to document it and to insert *assert()* statements verifying correct alignment. I did that in the [boxlambda branch of the BoxLambda fork of SDSPI](https://github.com/epsilon537/sdspi/tree/boxlambda/sw).

FatFs Configuration
===================
FatFs is very configurable, so you can trade options for footprint.
All configuration options are well-documented and centralized in the *ffconf.h* file.
Relative to the default settings, I modified the following:
- **Enable FF_USE_FIND**: filtered directory read functions, *f_findfirst()* and *f_findnext()*.
- **Enable FF_USE_STRFUNC**: string functions, *f_gets()*, *f_putc()*, *f_puts()*, and *f_printf()*.
- **Enable FF_FS_RPATH**: support for relative paths.
- **Enable FF_FS_NORTC**: I *disabled* the timestamp feature. I will revisit this when I bring up RTC on BoxLambda.

[https://github.com/epsilon537/fatfs/blob/boxlambda/source/ffconf.h](https://github.com/epsilon537/fatfs/blob/boxlambda/source/ffconf.h)

FatFs_Test
==========
FatFs itself does not provide a test suite, but I found a simple test sequence in [another project](https://github.com/avrxml/asf/blob/master/thirdparty/fatfs/unit_tests/unit_tests.c). I used that code as the starting point for a BoxLambda [fatfs_test](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/fatfs_test/fatfs_test.c).

With the endianness fix in place, *fatfs_test is* working fine, both in Verilator and on FPGA.

See [instructions below](#fatfs-test-on-verilator) for building and running the *fatfs_test* project.

Memory Footprint
----------------
The *fatfs_test* code together with the FatFs library and the Picolibc library still fits in BoxLambda's on-chip RAM, but it is getting tight. On the Arty A7-35T, we have 64KB of on-chip RAM. BoxLambda's current link map allocates 32KB to code and Read-Only data and 32KB to Read-Write data. The *fatfs_test* program currently uses 28KB of code and read-only data and 4KB of read-write data.


|                        | Code (KB) | RO-Data (KB) | RW-Data (KB) |
| ---------------------- | ----------| ------------ | ------------ |
| Test App + UART driver | 1.8       | 0.4          | 3.1          |
| Picolibc + GCC         | 9.6       | 1.3          | 0.1          |
| FatFs                  | 14.1      | 0.8          | 0.3          |
| Stack                  | 0         | 0            | 0.5          |
| Total                  | 25.5      | 2.5          | 4.0          |

Relevant Files and Directories
------------------------------
- [gw/projects/sdspi_test/rtl/sdspi_test_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/sdspi_test/rtl/sdspi_test_soc.sv): The SDSPI Test SoC top-level module instantiating the SDSPI core.
- [sw/projects/sdspi_test/sdtest.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/sdspi_test/sdtest.c): A test program verifying low-level SD Card access using the SDSPI core.
- [sw/projects/fatfs_test/fatfs_test.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/fatfs_test/fatfs_test.c): A test program verifying file system level access to the SD Card, using the FatFS library with SDSPI bindings.
- [gw/projects/sdspi_test/](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/sdspi_test): BoxLambda test build based on the SDSPI Test SoC, running the *sdtest.c* program.
- [gw/projects/fatfs_test/](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/fatfs_test): BoxLambda test build based on the SDSPI Test SoC, running the *fatfs_test.c* program.
- [sub/sdspi/](https://github.com/epsilon537/sdspi/tree/boxlambda): BoxLambda git submodule referencing the BoxLambda fork of the SDSPI repo. Note that the selected branch in this repo is *boxlambda*.
- [sub/fatfs/](https://github.com/epsilon537/fatfs/tree/boxlambda): BoxLambda git submodule referencing the BoxLambda fork of the FatFS repo. Note that the selected branch in this repo is *boxlambda*.
- [sw/components/fatfs/](https://github.com/epsilon537/boxlambda/tree/master/sw/components/fatfs): BoxLambda software component to be used by software project builds wishing to use FatFS.
- [gw/components/sdspi/](https://github.com/epsilon537/boxlambda/tree/master/gw/components/sdspi): BoxLambda gateware component to be used by gateware project builds wishing to include the SDSPI core.

Try It Out
----------

Setup
=====

1. Install the [Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
2. Get the BoxLambda repository:
	```
	git clone https://github.com/epsilon537/boxlambda/
	cd boxlambda
	```
3. Switch to the *sd_and_fs* tag:
	```
	git checkout sd_and_fs
	```
4. Set up the repository. This initializes the git submodules used and creates the default build trees:
	```
	./boxlambda_setup.sh
	```

SDSPI Test on Verilator
=======================
1. Build the *sdspi_test* project:
	```
	cd build/sim/gw/projects/sdspi_test
	make sdspi_test_sim
	```
1. Create and format the SD Card image file (or just use the *sdcard.img* files checked into the *test/* subdirectory of the *sdspi_test* project):
	```
    dd if=/dev/zero of=sdcard.img bs=512 count=131072
	mkfs.fat -F 16 sdcard.img
	```
2. Execute the generated Verilator model. Pass in the *sdcard.img* file:
	```
	./Vmodel -s sdcard.img
	```
3. You should now see the following messages appear in the terminal window. The traces prefixed with *SDSPI:* come from the SDSPI co-simulator. The first two lines and the last line come from the test bench. The other lines are *printf()* statements coming from the test program running on the RISCV processor.

	```
	SD Image File: /home/epsilon/sdcard.img
	SDCARD: NBLOCKS = 131072


	SDSPI testing program

	Initializing the SD-Card
	SDSPI: CMDIDX = 6 -- WE HAVE A COMMAND # 0! [ 40 00 00 00 00 95 ]
	SDSPI: Received a command 0x40 (0) arg 0x0
	...
	READ: Seek to sector 3
	Write sector 2
	Read sector 3
	SDSPI: CMDIDX = 6 -- WE HAVE A COMMAND #17! [ 51 00 00 00 02 71 ]
	SDSPI: Received a command 0x51 (17) arg 0x2
	Reading from block 00000002 of 00020000
	READ: Seek to sector 2
			Ctrl-RSP: 00000000
	Read sector 2
	Test is complete
	SDSPI Test successful.
	```

SDSPI Test on Arty A7
======================
1. Hook up Digilent's [MicroSD PMOD](https://digilent.com/shop/pmod-microsd-microsd-card-slot/) to port **JD** and insert an SD card into the card reader.

   Note that this is a destructive test. The contents of the SD card will be destroyed.

2. Build the *sdspi_test* project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
	```
	cd build/arty-a7-35/gw/projects/sdspi_test
	make sdspi_test_impl
	```
3. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.
4. Download the generated bitstream file to the Arty A7:
	```
	make sdspi_test_load
	```
5. Verify the test program's output in the terminal. You should see something like this:

	```
	SDSPI testing program

	Initializing the SD-Card
	CMD0 - the INIT command
	Testing the AUX register
	CMD1 - SEND_OP_COND, send operational conditions (voltage)
	CMD8 - SEND_IF_COND, send interface condition
	...
	Write sector 2
	Read sector 3
			Ctrl-RSP: 00000000
	Read sector 2
	Test is complete
	SDSPI Test successful.
	```

FatFS Test on Verilator
=======================
1. Build the *fatfs_test* project:
	```
	cd build/sim/gw/projects/fatfs_test
	make fatfs_test_sim
	```
2. Create and format the SD Card image file (or just use the *sdcard.img* files checked into the *test/* subdirectory of the *fatfs_test* project):
	```
    dd if=/dev/zero of=sdcard.img bs=512 count=131072
	mkfs.fat -F 16 sdcard.img
	```
3. Execute the generated Verilator model. Pass in the *sdcard.img* file:
	```
	./Vmodel -s sdcard.img
	```
4. You should see the following messages in the terminal window. The traces prefixed with *SDSPI:* come from the SDSPI co-simulator. The first two lines and the last line come from the test bench. The other lines are *printf()* statements coming from the test program running on the RISCV processor.
	```
	SD Image File: /home/epsilon/sdcard.img
	SDCARD: NBLOCKS = 131072
	Starting fatfs_test...
	SDSPI: CMDIDX = 6 -- WE HAVE A COMMAND # 0! [ 40 00 00 00 00 95 ]
	SDSPI: Received a command 0x40 (0) arg 0x0
	...
	SDSPI: TOKEN!!
	LEN = 514
	CHECKING CRC: (rx) f8cd =? f8cd (calc)
	FatFS Test Completed Successfully!
	Test passed.
	```
5. One of the steps taken by the test program is to create a **LOG.TXT** file with the contents *This is a test*. We can mount the *sdcard.img* on Linux and check if that file exists with the expected contents:
	```
	sudo mount -o loop ~/sdcard.img /mnt/sd
	ls -al /mnt/sd
	cat /mnt/sd/LOG.TXT
	This is a test.
	sudo umount /mnt/sd
	```

FatFS Test on Arty A7
=====================
1. Hook up Digilent's [MicroSD PMOD](https://digilent.com/shop/pmod-microsd-microsd-card-slot/) to port **JD** and insert a FAT-formatted SD card into the card reader.

   Note that this is a destructive test. The contents of the SD card will be destroyed.

2. Build the *fatfs_test* project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
	```
	cd build/arty-a7-35/gw/projects/fatfs_test
	make fatfs_test_impl
	```
3. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.
4. Download the generated bitstream file to the Arty A7:
	```
	make fatfs_test_load
	```
5. Verify the test program's output in the terminal. You should see something like this:
	```
	Starting fatfs_test...
	Mounting...
	CID: 534d5402:47323341:7d604971:3168018d
	Opendir...
	Creating file...
	Writing...
	Closing file...
	Re-opening file for reading...
	Reading...
	Closing file...
	Comparing...
	f_printf test...
	FatFS Test Completed Successfully!
	```
6. One of the steps taken by the test program is to create a **LOG.TXT** file with the contents *This is a test*. Eject the SD card, insert it into your PC, and verify that *LOG.TXT* exists with the expected contents.

Other Changes
-------------
I updated the *Picolibc* installation for BoxLambda to version 1.8.1. This made it easier to use *assert()* statements.

Conclusion
----------
Bringing up an SD Card controller and a file system turned out to be straightforward thanks to Dan Gisselquist's [SDSPI](https://github.com/ZipCPU/sdspi) repo. The repo not only contains an SDSPI core, but also a test bench with a co-simulator, excellent documentation, and low-level driver code to hook into [FatFs](http://elm-chan.org/fsw/ff/00index_e.html). To top it off, FatFs itself expects nothing more than a minimal C environment, so it compiles out of the box in virtually any environment.

Next up is **Sound**. I suspect that nut won't be so easy to crack.
