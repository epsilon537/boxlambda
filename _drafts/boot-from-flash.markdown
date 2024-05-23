---
layout: post
title: 'SPI Flash Access, Boot, and Core.'
comments: true
---

![SPI Flash On On BoxLambda](../assets/spiflash_on_boxlambda.png)

*SPI Flash on BoxLambda.*

BoxLambda now supports booting software and gateware directly from Flash. Flash read and write access from software running on the Ibex RISCV processor is supported as well. This post describes the steps taken to add these features. As usual, I'm leveraging an existing open-source core plus accessories.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_Flash_Focus.png)

This is a summary of the current state of affairs for BoxLambda. We have:
- An Ibex RISC-V core, Wishbone Interconnect, timer, two GPIO ports, UART core, and internal memory.
- DDR3 external memory access through the Litex Memory Controller.
- OpenOCD-based Debug Access, both on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- USB HID Keyboard and Mouse support.
- A PicoRV32-based Programmable DMA Controller.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- Test builds running on Arty-A7-35T, Arty-A7-100T, Verilator, and CocoTB.
- A Linux CMake and Bender-based Software and Gateware build system with support for automated testing and post-implementation memory updates.

Flashing a Bitstream to the Arty A7
-----------------------------------
So far, I've been loading bitstream files onto the FPGA using JTAG. Xilinx FPGA devices also have built-in support for loading the bitstream file from SPI Flash memory, however.
[The Arty A7 board supports this boot-from-flash mechanism](https://digilent.com/reference/programmable-logic/arty-a7/reference-manual#quad-spi_flash). It has a 16MB quad-SPI flash device on board. Through a jumper setting (SP1) you can choose to boot the FPGA from flash at power-up or to wait for configuration via JTAG.

![Arty A7 Configuration](https://digilent.com/reference/_media/arty/arty_cfg.png)

*Arty A7 Configuration (source: Arty A7 Reference Manual)*.

Bitstream files are programmed into these flash devices using an [Indirect Programming](https://docs.amd.com/r/en-US/xapp586-spi-flash/Introduction) method: Via JTAG, an FPGA application containing a SPI flash core is loaded onto the FPGA. Then, through this application, the bitstream is flashed onto the device. The Vivado Hardware Manager handles the details behind the scenes.

Working with the Vivado Hardware Manager, whether through the GUI or the Tcl interface, is a bit slow and clunky, however. Yosys' [OSS CAD Suite](https://github.com/YosysHQ/oss-cad-suite-build), one of BoxLambda's prerequisites, includes a more convenient utility called **openFPGALoader**. To flash a bitstream onto the Arty A7, just run the following command:

```
openFPGALoader -b arty_a7_100t -f <bitstream file>
```

OpenFPGALoader is also available separately [here](https://github.com/trabucayre/openFPGALoader).

For convenience, I added a \<project\>_flash_gw target to the build system. E.g.:

```
make ddr_test_bit_sw #Build the bitstream file
make ddr_test_flash_gw #Flash the bitstream file onto the Arty A7.
```

Accessing Flash from an FPGA Application
----------------------------------------
Only 4MB of the available 16MB is needed to hold the bitstream file. The remaining 12MB can be used to store software images and other data (fonts, logos, system settings...).
If, after booting the bitstream, you want to be able to access flash memory, you need to include a SPI Flash core into the FPGA application.

SPI Flash Core Considerations
=============================

#### Quad-SPI Flash vs. Single-SPI Flash

![Arty A7 SPI Flash](https://digilent.com/reference/_media/arty/arty_spiflash.png){:height="200px" width="400px"}

*Arty A7 SPI Flash (source: Arty A7 Reference Manual)*.

The Arty A7 has a quad-SPI flash device, so using a quad-SPI flash core would make sense. I chose to use a single-SPI flash core, however, for a couple of reasons:

- A single-SPI flash core is less complex than a quad-SPI flash core. single-SPI flash is also slower than quad-SPI flash, but I don't have a specific performance requirement for flash access. I do have a simplicity requirement, so I choose to trade performance for simplicity.
- The flash device used on the Arty A7 depends on the PCB revision. Some revisions use Micron, other revisions use Spansian. Quad-SPI flash access is not standardized across SPI flash devices, however. E.g. the command sequence needed to get the device into quad-SPI mode is device-dependent. The latency between a read command and the resulting data is device-dependent as well in quad-SPI mode. In single-SPI mode, these devices behave the same (at least when it comes to the limited feature set I'll be using in BoxLambda).

![Arty A7 PCB revisions and their flash devices](../assets/arty_flash_devices.jpg)

*Arty A7 PCB revisions and their flash devices.*

#### Memory Mapped Read Access

I would like to be able to boot software from flash memory. That means that the CPU should be able to read from flash memory as if it were a regular ROM module. The flash core should present to the SoC a memory-mapped Wishbone read interface that can address the entire 16MB flash memory range.

#### Writing to Flash

I would like to be able to write to flash memory. The flash core should include a Wishbone control interface through which a driver can issue SPI flash commands.

#### Configurable SPI Clock Frequency

To have a bit more control over timing, I would like to be able to configure a SCK clock frequency relative to the system clock frequency.

#### Simulating Flash

As always, I want to simulate the entire setup in Verilator. I'll need to attach a SPI Flash co-simulation model to the testbench.

The ZipCPU SpiXpress core
=========================
ZipCPU's spixpress core meets all of the above requirements, except the configurable SCK frequency bit. It's a single-SPI core with a Wishbone read interface as well as a control interface. The repository also includes driver software for the SPI core and a C++ SPI Flash device co-simulation model for Verilator. Here is the GitHub repository:

[https://github.com/ZipCPU/qspiflash](https://github.com/ZipCPU/qspiflash)

The repository is named *qspiflash* but it includes a quad-SPI, Dual-SPI, and single-SPI core (and a co-simulation model supporting all three).

Dan Gisselquist wrote this article describing the design of the single-SPI core:

[https://zipcpu.com/blog/2018/08/16/spiflash.html](https://zipcpu.com/blog/2018/08/16/spiflash.html)

![SPI Flash Core Diagram](../assets/spiflash_core_diagram.png)

*The simplified SpiXpress SPI Flash Core Design.*

The BoxLambda SPI Flash Core Variant
====================================

BoxLambda's SPI Flash core uses the ZipCPU spixpress core as a starting point. I made the following changes relative to the original core:
- The SCK output port of the core is the actual SPI clock signal, rather than an enable signal to be used in conjunction with a DDR primitive.
- I added a clock divider parameter for SCK. I'm using a clock divider value of two in the BoxLambda SoC.
- The core shifts out the serial output data at the SCK falling edge and shifts in the serial input data at the SCK rising edge. I modified the **Flashsim** co-simulator module to behave like this as well. This is the standard SPI timing design.

![SPI Timing Design](../assets/spi_rising_falling_edge.png)

- I removed the pipeline logic.
- BoxLambda is a little-endian platform. When reading 32-bit words from Flash, the input is shifted in in little-endian fashion.

The BoxLambda version of the Spiflash core and Flashsim co-simulator can be found here:

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/spiflash/rtl/spiflash.v](https://github.com/epsilon537/boxlambda/blob/master/gw/components/spiflash/rtl/spiflash.v)

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/spiflash/sim/flashsim.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/components/spiflash/sim/flashsim.cpp)

Reading from Flash - the Data Interface
=======================================
The SPI Flash core has a 32-bit Wishbone read interface. Through this interface, the user can request the core to read 32-bit words at a time from Flash memory. At SPI level, the transaction looks like this:

![Word read SPI Flash Transaction](../assets/spiflash_word_read.png)

*Reading a 32-bit word from SPI Flash. 8 (C)ommand bits, followed by 24 (A)ddress bits, followed by 32 (D)ata bits.*

- *C7-C0*: Command Bits. The Read Command ID is 0x03.
- *A23-A0*: Address Bits covering the entire 16MB address range.
- *D31-D0*: Data Bits.
- *cs_n*: Active Low Chip Select of the SPI device.
- *sclk*: SPI Clock.
- *mo*: Master Out.
- *si*: Slave In.
- *so*: Slave Out.
- *mi*: Master In.

The Control Interface and Flash Driver
======================================
The SPI Flash core has a simple but clever control interface (invented by Dan, not me). From the *spiflash.v* header:

```
//     Control Port
//     [31:9]    Unused bits, ignored on write, read as zero
//     [8]    CS_n
//             Can be activated via a write to the control port.
//             This will render the memory addresses unreadable.
//             Write a '1' to this value to return the memory to
//             normal operation.
//     [7:0]    BYTE-DATA
//             Following a write to the control port where bit [8]
//             is low, the controller will send bits [7:0] out the
//             SPI port, top bit first.  Once accomplished, the
//             control port may be read to see what values were
//             read from the SPI port.  Those values will be stored
//             in these same bits [7:0].
```

I.e. the control port consists of a single 9-bit register. By setting the CS_n bit to 0, software can choose to 'grab' the SPI Flash port and keep ownership of it for multiple SPI transactions. When done, software releases the SPI Flash port again by setting CS_n to one in the control register.

As an example, the Flash Driver code sequence to read the Flash Device ID looks like this:

```
    /*Write Flash ID Command Byte*/
    m_fpga->writeio(FLASHCFG, CFG_USERMODE | 0x9f);

    /*Write 0 Byte - While this gets sent to SPI Flash,
     *8-bits of miso response get shifted in.*/
    m_fpga->writeio(FLASHCFG, CFG_USERMODE | 0x00);

    /*Read the byte that got shifted in.*/
    r = m_fpga->readio(FLASHCFG) & 0x0ff;

    /*Write another 0 Byte - so we can retrieve the next response byte.*/
    m_fpga->writeio(FLASHCFG, CFG_USERMODE | 0x00);

    /*Read the byte that got shifted in.*/
    r = (r<<8) | (m_fpga->readio(FLASHCFG) & 0x0ff);

    /*Etc, until the 4-byte response to the Flash Id. command is retrieved.*/
    m_fpga->writeio(FLASHCFG, CFG_USERMODE | 0x00);
    r = (r<<8) | (m_fpga->readio(FLASHCFG) & 0x0ff);
    m_fpga->writeio(FLASHCFG, CFG_USERMODE | 0x00);
    r = (r<<8) | (m_fpga->readio(FLASHCFG) & 0x0ff);
    m_id = r;
```

The primary purpose of the Control Interface is to allow software to write to Flash. One does not simply write a byte to flash, however. The appropriate sector needs to be identified, erased, and paged (written). The **flashdrvr** software module implements this logic. BoxLambda's *flashdrvr* module is a simplified version of the one found in the *qspiflash* repo. All the logic related to Quad- or Dual-SPI flash support has been removed.

[https://github.com/epsilon537/boxlambda/tree/master/sw/components/flashdrvr](https://github.com/epsilon537/boxlambda/tree/master/sw/components/flashdrvr)

Timing
======
Is an SCK frequency of 25MHz slow enough to stay out of trouble? I took a look at the timing.
For MOSI timing, I'm taking into account the following delays:
- The Clock-to-Out delay in the FPGA's output flip-flop OLOGIC: 0.5ns
- The FPGA IOB pad output delay: 4ns.
- Estimated trace propagations delay assuming a signal speed of 15cm/ns: 0.5ns
- SPI Flash setup and hold time requirement: 2ns / 3ns

![SPI Flash MOSI Timing](../assets/spi_flash_mosi_timing.png)

*SPI Flash MOSI Timing.*

I end up with 18ns of slack. That's plenty.

For MISO timing, I'm taking into account the following delays:
- The Clock-to-Out delay in the FPGA IO Tile's output flip-flop OLOGIC: 0.5ns
- The FPGA IOB pad output delay: 4ns.
- Estimated trace propagations delay assuming a signal speed of 15cm/ns: 0.5ns
- SPI flash Clock Low to Output valid delay: 8ns
- The FPGA IOB pad input delay: 1.5ns

![SPI Flash MISO Timing](../assets/spi_flash_miso_timing.png)

*SPI Flash MISO Timing.*

Here I get only 5ns of slack. That's much less than I expected, but should still be good enough.

The proper way to check timing would be to constrain the SPI Flash IO ports and use Vivado Timing Analysis. I'll need to do this for the other IO ports used by BoxLambda as well. I made a note of it.

FPGA Utilization
================
The SPI Flash core is tiny:

**Slice LUTs**: 48

**Slice Registers**: 77

Booting Software from Flash
---------------------------
To be able to boot a software image from flash memory, I need to define a new boot sequence. The boot sequence is split across two modules:
- [vectors.S](): Assembly code containing the Boot Vector and early startup code (nothing more than setting the CPU registers to zero, really).
- **crt0** ([.c](https://github.com/epsilon537/boxlambda/tree/master/sw/components/bootstrap/crt0.c) and [.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/crt0.h)): Contains the bulk of the startup code, including relocation and initialization of code, data, and BSS segments. Crt0 sets up the "C" environment. So far, I've been using the crt0 module that comes with picolibc, but because customization is required for the flash boot sequence, I put the BoxLambda variant in [sw/components/bootstrap/](https://github.com/epsilon537/boxlambda/tree/master/sw/components/bootstrap).

The flash boot sequence looks like this:

![The Software Boot Sequence](../assets/spiflash_boot_sequence.png)

*The Software Flash Boot Sequence.*

Note that, technically, BoxLambda doesn't boot from Flash Memory. It boots from CMEM at address offset 0x80. There, it executes the early startup code defined in *vectors.S* before jumping to the CRT0 code located in Flash Memory.
I could have just pointed the Ibex Boot Vector to Flash Memory, but because I already have so many test programs that execute directly from CMEM, I decided to keep the Boot-from-CMEM feature, and from there branch to Flash Memory if so desired. The CMEM software program that just branches to the flash boot code (i.e. containing just *vectors.S*) is located here:

[https://github.com/epsilon537/boxlambda/tree/master/sw/projects/cmem_to_flash_vector](https://github.com/epsilon537/boxlambda/tree/master/sw/projects/cmem_to_flash_vector)

The Linker Script
=================
When building a software executable, the GCC linker executes a *Linker Script*. The Linker Script defines the following:
- Relevant Memories on the target device: In the case of BoxLambda, these are *cmem*, *dmem*, *emem* (=DDR memory), and *flash*.
```
MEMORY
{
    flash : ORIGIN = __flash, LENGTH = __flash_size
    cmem : ORIGIN = __cmem, LENGTH = __cmem_size
    dmem : ORIGIN = __dmem, LENGTH = __dmem_size
    emem : ORIGIN = __emem, LENGTH = __emem_size
}
```
- The mapping of input to output sections. Input sections are defined in the source code and default to .text, .bss, and .data when not explicitly specified. The output sections for BoxLambda are: *.flash*, *.text*, *.cmem_bss*, *.data*, *.tdata*, *.tbss*, *.bss*, *.heap* and *.stack*.
```
    .text : {
        ...
        *(.text.unlikely .text.unlikely.*)
        *(.text.startup .text.startup.*)
        *(.text .text.*)
        *(.gnu.linkonce.t.*)
    ...
```
- The mapping of output sections to memories and, for sections that require relocation, the memory from which to load them. For BoxLambda software executables built to boot from Flash, the load memory will be the *flash* memory.
```
    ...
    .text : {...
    } >cmem AT>flash
    ...
    .data : ALIGN_WITH_INPUT {...
    } >dmem AT>flash
    ...
    .bss (NOLOAD) : {
    } >dmem
```
  - Code sections go to *cmem*.
  - Data, BSS, and stack sections go to *dmem*.
  - The *.cmem_bss* section goes to *cmem*.
  - The heap goes to *emem*.
- Symbols to be used by the CRT0 code for section relocation, BSS initialization, etc. For BoxLambda, the key symbols are:
  - *__code_source / __code_start / __code_size*: source address, destination address, and size of the code section to relocate from flash to CMEM.
  - *__data_source / __data_start / __data_size*: source address, destination address, and size of the data section to relocate from flash to DMEM.
  - *__bss_start / __bss_size*: Address and size of BSS section in DMEM to zero out.
  - *__cmem_bss_start / __cmem_bss_size*: Address and size of BSS section in CMEM to zero out.
```
    .text : {
        PROVIDE(__code_start = ADDR(.text));
        ...
        PROVIDE(__code_end = .);
    } >cmem AT>flash
    PROVIDE(__code_source = LOADADDR(.text));
    PROVIDE(__code_size = __code_end - __code_start );
    ...
```

BoxLambda has two linker scripts:
- [link_cmem_boot.ld](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/link_cmem_boot.ld): For software images that boot from CMEM.
- [link_flash_boot.ld](https://github.com/epsilon537/boxlambda/blob/master/sw/components/bootstrap/link_flash_boot.ld): For software images that boot from Flash.

Note that BoxLambda no longer has separate linker scripts for the Arty-A7-35T and the Arty-A7-100T. The CMEM and DMEM sizes are passed to the linker via symbols defined by the build system. These symbols no longer have to be defined inside the linker scripts, so the same linker scripts can be used for the Arty-A7-35T and the Arty-A7-100T.

```
  #Pass linker script and picolibc specs to linker, and define __cmem_size and __dmem_size symbols.
  target_link_options(${_tgt}
        PRIVATE
            -T${_link_script}
            "--specs=${CMAKE_BINARY_DIR}/sw/picolibc.specs"
            "LINKER:--defsym=__cmem_size=${CMEM_SIZE}"
            "LINKER:--defsym=__dmem_size=${DMEM_SIZE}"
            "LINKER:--gc-sections"
            "LINKER:--Map,${CMAKE_CURRENT_BINARY_DIR}/${_tgt}.map"
    )
```

The Test Build
--------------
The **spiflash_test** program does the following:

1. Boot from Flash Memory.
2. Read the Flash ID.
3. Write a character string to a specific Flash Memory location.
4. Read back the character string from Flash Memory and compare it against the written string.
5. Write a different character string of the same length to the same Flash Memory location.
6. Read back the character string from Flash Memory and compare it against the written string.

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/spiflash_test/spiflash_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/spiflash_test/spiflash_test.cpp)

Other Changes
-------------

ST Sound Test Failure Fixed - An Unexpected LiteDRAM Issue
===========================================================
![LiteDRAM Back-to-Back Write Issue](../assets/litedram_b2b_write_issue.png)

*LiteDRAM Back-to-Back Write Issue.*

For a while now, the ST Sound regression test has been failing. I finally got around to looking into the issue. I expected to find a trivial software bug, but I ran into a serious LiteDRAM issue.

The ST Sound YM Song Decoder performs a deinterleaving of the YM song data (like a row-column transposition). Some song data bytes got corrupted during the execution of the deinterleaving routine. I expected to find a coding error in this routine, but that wasn't the case. Luckily, I was able to reproduce the issue in simulation, so I could dig as deep as needed to find the root cause. It turns out that, if the LiteDRAM controller receives two write requests in a single Wishbone CYC transaction, the 2nd write request gets stuck in a write FIFO. From then on, the LiteDRAM controller will be running one transaction 'behind', i.e. any new write request pushes out the previous write request, but the new request gets stuck in the write FIFO.

Luckily, the workaround is easy. In the LiteDRAM Wishbone adapter, rather than flush out writes only between CYCs, flush them out between every read or write transaction. In LiteDRAM's *wishbone.py*:

```
self.comb += [
port.cmd.addr.eq(wishbone.adr - offset),
port.cmd.we.eq(wishbone.we),
port.cmd.last.eq(~wishbone.we), # Always wait for reads.
port.flush.eq(1) # Always flush.
]
```

The issue hasn't been confirmed yet by LiteDRAM's maintainers. I opened a case:

[https://github.com/enjoy-digital/litedram/issues/358](https://github.com/enjoy-digital/litedram/issues/358).

Ibex and RiscV-Dbg codebase updates
===================================
I updated the Ibex and Riscv-Dbg submodules to the latest codebase. This fixes an issue where Riscv-Dbg issued certain *csr* instructions that Ibex didn't support.

OpenOCD 0.12
============
I switched to OpenOCD 0.12 and updated the OpenOCD debug configuration scripts where necessary. OpenOCD 0.12 comes with the *oss-cad-suite*, a BoxLambda prerequisite, so BoxLambda no longer needs to depend on the *riscv-openocd* repo.

I created some convenience scripts:

- *scripts/openocd_verilator.sh*: Starts an OpenOCD session that attaches to a Verilator model expecting a debugger to attach (*Vmodel -a*).
- *scripts/openocd_arty_a7_35.sh*: Starts an OpenOCD session that attaches to BoxLambda running on an attached Arty A7 35T.
- *scripts/openocd_arty_a7_100.sh*: Starts an OpenOCD session that attaches to BoxLambda running on an attached Arty A7 100T.

Try It Yourself
---------------

Setup
=====
1. Install the [Software Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
2. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
1. Switch to the **spi_flash** tag:
```
git checkout spi_flash
```
1. Set up the repository. This initializes the git submodules used and creates the default build trees:
```
./boxlambda_setup.sh
```

The SPI Flash Test on Verilator
===============================
1. Build the **spiflash_test** gateware project. This will also build the *spiflash_test.bin* software image as a dependency:
```
cd build/sim-a7-100/gw/projects/spiflash_test
make spiflash_test_sim_sw
```

1. Execute the generated Verilator model, passing in as a parameter the *spiflash_test.bin* software image. The software image will be loaded into the simulated flash device before the test starts:
```
./Vmodel -f ../../../sw/projects/spiflash_test/spiflash_test.bin
Flash SW Image File: ../../../sw/projects/spiflash_test/spiflash_test.bin
...
Starting test...
Reading one byte from FLASHBASE+0x800000:
Read back value = 0xff
flash id: 0x1152340
Writing to FLASHBASE+0x800000:
ERASING SECTOR: 11800000
Erasing sector: 800000
Writing page: 0x11800000 - 0x118000ff
Sector 0x11800000: DONE
Written [0]: 0x48
...
Written [11]: 0x2e
Written [12]: 0x0
Reading back from FLASHBASE+0x800000:
Read back [0]: 0x48
...
Read back [11]: 0x2e
Read back [12]: 0x0
Writing to FLASHBASE+0x800000:
ERASING SECTOR: 11800000
Erasing sector: 800000
Writing page: 0x11800000 - 0x118000ff
Sector 0x11800000: DONE
Written [0]: 0x2e
...
Written [11]: 0x48
Written [12]: 0x0
Reading back from FLASHBASE+0x800000:
Read back [0]: 0x2e
...
Read back [11]: 0x48
Read back [12]: 0x0
Test Successful.
Test passed.
```

The SPI Flash Test on FPGA
==========================
1. If you're running on WSL, check BoxLambda's documentation [On WSL](https://boxlambda.readthedocs.io/en/latest/installation/#on-wsl) section.
2. Connect a terminal program to Arty's USB serial port. **Settings: 115200 8N1**.
3. Build the *spiflash_test* software project in an Arty A7 build tree:
```
cd build/arty-a7-100/sw/projects/spiflash_test
make spiflash_test
```
4. Flash the *spiflash_test* program onto the target:
```
make spiflash_test_flash_sw
```
5. Build the gateware project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
```
cd build/arty-a7-100/gw/projects/spiflash_test
make spiflash_test_bit_sw
```
6. Flash the gateware build onto the target:
```
make spiflash_test_flash_gw
```
7. When flashing has been completed, the target will be reset. In the terminal, you should now see the same output as with the Verilator test build above.

Conclusion
----------
A single-SPI flash core is a tiny core that can add a lot of value to a project. Make sure you don't underestimate the timing details, however. The SPI protocol uses both clock edges and the delays involved with two devices communicating across a physical bus add up quickly. Next up is *interrupts*!

References
----------
[Spansion S25FL128S Flash Datasheet](https://www.infineon.com/dgdl/Infineon-S25FL128S_S25FL256S_128_Mb_(16_MB)_256_Mb_(32_MB)_3.0V_SPI_Flash_Memory-DataSheet-v18_00-EN.pdf?fileId=8ac78c8c7d0d8da4017d0ecfb6a64a17&utm_source=cypress&utm_medium=referral&utm_campaign=202110_globe_en_all_integration-files&_ga=2.126822669.1446861363.1715422103-205165240.1710706435)

[Micron N25Q128A13ESF40F Flash Datasheet](https://www.mouser.co.uk/datasheet/2/671/micts06184_1-2290782.pdf)

[Artix 7 Datasheet](https://docs.amd.com/v/u/en-US/ds181_Artix_7_Data_Sheet)

[7 Series FPGAs SelectIO Resources](https://doc.inmys.ru/open?path=ic%5Cfpga%5Cxilinx%5Cug471_7Series_SelectIO.pdf)

[Arty A7 Reference Manual](https://digilent.com/reference/programmable-logic/arty-a7/reference-manual)

[How to build a SPI Flash Controller for an FPGA](https://zipcpu.com/blog/2018/08/16/spiflash.html).

[ZipCPU's qspiflash repo](https://github.com/ZipCPU/qspiflash).

[Using SPI Flash with 7 Series FPGAs Application Note](https://docs.amd.com/r/en-US/xapp586-spi-flash/Using-SPI-Flash-with-7-Series-FPGAs-Application-Note-XAPP586)

