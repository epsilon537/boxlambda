Terms and Abbreviations
=======================

This section provides clarification for some of the more ambiguous terms and abbreviations used elsewhere in the documentation.

- **ACK**: A Wishbone Bus Signal.
  
- **AXI**: Advanced eXtensible Interface, ARM's SoC bus specification.

- **Bitstream**: An FPGA Bitstream is a file containing the programming data associated with an FPGA chip.

- **Blitter**: A type of DMA often used in the context of 2D graphics, copying, combining, and/or modifying bitmap graphics in video memory.

- **BPP**: Bits Per Pixel.

- **BSCANE**: A Xilinx primitive giving access to and from the FPGA's JTAG scan chain.

- **BSS**: Block Starting Symbol. Portion of an object file or executable holding zero-initialized data.

- **Code Segment**: Portion of an object file or executable holding executable instructions.

- **Console**: The physical terminal consisting of a screen, a keyboard, and optionally a mouse. Console I/O means input/output from/to these physically attached devices.

- **Constraints File**: A constraints file specifies the mapping of the top-level HDL module's input and output ports to the physical pins of the FPGA. It also defines the clocks used by the given design. See [https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file](https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file).

- **CPP**: C Preprocessor, a macro processor used automatically by the C compiler.

- **CPU**: Central Processing Unit.

- **CRT0**: C Run-Time 0. A set of execution startup routines linked into a C program that performs any initialization work required before calling the program's *main()* function. 

- **CSR**: Control and Status Register.
  
- **CYC**: A Wishbone Bus Signal.

- **DAC**: Digital-to-Analog Converter.

- **.data**/**Data Segment**: Portion of an object file or executable holding pre-initialized data.

- **DDR SDRAM**: Double Data Rate SDRAM.

- **DFX**: Dynamic Function Exchange, Xilinx's solution for Partial FPGA Reconfiguration ([https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf))

- **DMA**: Direct Memory Access, a hardware assist component offloading memory copy operations from the CPU.

- **DSP**: Digital Signal Processing.

- **DMAC**: DMA Controller.

- **Double Buffering**:  A technique for drawing graphics that shows no stutter, tearing, or other rendering artifacts. Buffer A is being displayed while buffer B is being updated.
  
- **DPRAM**: Dual-Port RAM.

- **DTM**: Debug Transport Module.

- **DUT**: Device Under Test.

- **EDA tool**: A software tool to design electronic circuits, e.g. Vivado.

- **ERR**: A Wishbone Bus Signal.
  
- **FHDL**: Fragmented Hardware Description Language, a Python-based HDL. Used by Migen and LiteX.

- **FIFO**: First-In-First-out, an implementation of a queue.

- **Fork**: A GitHub fork is a copy of a repository that sits in your account rather than the account from which you forked the data.

- **FTDI**: Future Technology Devices International Inc. The name has become synonymous with the USB-to-UART adapter ICs sold by this company.
  
- **Gateware**: [Gateware comprises the description (of behavior, structure and/or connections) of digital logic gates, a high-level abstraction thereof, and/or the implementation thereof in (re)configurable logic devices (such as FPGAs and ASICs)](https://www.gateware.org/definition-of-gateware).

- **GCC**: GNU Compiler Collection. An optimizing compiler produced by the GNU Project.

- **GPIO**: General-Purpose Input/Output, an uncommitted pin used for input and/or output controllable by the user at run-time.

- **Hacker/Hacking**: See [http://www.paulgraham.com/gba.html](http://www.paulgraham.com/gba.html)

- **Ibex**: The name of the RISC-V CPU core used by BoxLambda.

- **I/F**: Interface.
  
- **IC**: Integrated Circuit.

- **Interconnect**: Wishbone terminology for the bus fabric.

- **IO**: Input/Output.

- **IP-XACT**: An XML format that defines and describes individual, re-usable electronic circuit designs to facilitate their use in creating integrated circuits.

- **IP Package**: A Vivado file encapsulating an IP component using the IP-XACT file format.

- **IRQ**: Interrupt Request.

- **ICAP**: Internal Configuration Access Port, a module giving access to the FPGA configuration functionality built into Xilinx FPGAs ([https://www.xilinx.com/products/intellectual-property/axi_hwicap.html](https://www.xilinx.com/products/intellectual-property/axi_hwicap.html))

- **ISA**: Instruction Set Architecture. The Instruction Set Architecture is the part of the processor that is visible to the programmer.

- **JTAG**: Joint Test Action Group, a standard designed to assist with device, board, and system testing, diagnosis, and fault isolation. Today JTAG is used as the primary means of accessing sub-blocks of ICs, making it an essential mechanism for debugging embedded systems.

- **JTAG DTM**: JTAG based Debug Transport Module.

- **JT49**: The name of Jotego's YM2149 compatible sound core implementation.

- **Linting**: Static Code Analysis.

- **LiteDRAM**: A small-footprint and configurable DRAM core. Part of LiteX.

- **LiteX**: A Migen and Python-based SoC Builder framework.
    
- **LUT**: Look-Up Table.

- **Makefile**: A file used by the *Make* utility, defining a set of tasks to be executed, and defining dependencies between tasks. Makefiles are commonly used to create build systems.

- **Memory File**: A file containing the initial contents of a Block RAM instance used in an FPGA design.

- **Memory Mapped IO**: Memory-mapped I/O uses the same address space to address both main memory and I/O devices. The memory and registers of the I/O devices are mapped to (associated with) address values. So a memory address may refer to either a portion of physical RAM or instead to the memory and registers of the I/O device.
  
- **MIG**: Memory Interface Generator, a parameterizable Xilinx IP module used to generate a Memory Controller.

- **MEMC**: Memory Controller.

- **Migen**: A Python-based toolbox for building digital hardware. Built on FHDL.
  
- **OOC**: Vivado's OOC mode or OOC flow lets you synthesize, implement, and analyze design modules in a hierarchical design.

- **OpenOCD**: Open On-Chip Debugger, open-source software that interfaces with a hardware debugger's JTAG port.

- **PIT**: Programmable Interval Timer.

- **PMOD**: Peripheral Module Interface, an open standard defined by Digilent for connecting peripheral modules to an FPGA.

- **Praxos**: The name of the DMA Controller used by BoxLambda.

- **PSG**: Programmable Sound Generator.

- **PWM**: Pulse Width Modulation.

- **Repo**: Repository.

- **RP**: Reconfigurable Partition. Part of Xilinx's DFX solution.

- **RM**: Reconfigurable Module. Part of Xilinx's DFX solution.

- **RTL**: Register-Transfer Level, an abstraction of a Digital Design, usually captured using a Hardware Description Language such as Verilog, SystemVerilog, or VHDL.

- **RV32IMCB**: Risc-V 32-bit Processor Variant with Multiplier/Divider, Compressed ISA, and Bit Manipulating Extensions.

- **Scan Line**: One line in the raster scanning pattern of the VGA display.

- **SDL**: Simple DirectMedia Layer is a cross-platform development library designed to provide low-level access to audio, keyboard, mouse, joystick, and graphics.
  
- **SDRAM**: Synchronous dynamic random-access memory. A DRAM where the operation of its external pin interface is coordinated by an externally supplied clock signal. 

- **Slice**: The basic logical unit of a Xilinx FPGA.

- **(Software) Image**: Snapshot of computer memory contents stored as a file.

- **SoC**: System-on-a-Chip. A System-on-a-Chip is an integrated circuit that integrates all or most components of a computer or other electronic system.

- **SPI**: Serial Peripheral Interface, a synchronous serial communication interface specification used for short-distance communication.

- **Sprite**:  A computer graphic that may be moved on-screen and otherwise manipulated as a single entity.
  
- **STB**: A Wishbone bus signal.

- **Stderr**: Standard Error. Name of the file object in the Standard C library associated with the standard error device.

- **Stdin**: Standard Input. Name of the file object in the Standard C library associated with the standard input device.

- **Stdout**: Standard Output. Name of the file object in the Standard C library associated with the standard output device.

- **Stdio**: Standard Input and Output. *Stdio.h* is the Standard C Library header file containing input and output functions such as printf() and scanf().

- **Synthesis**: Synthesis turns a module's Verilog/System Verilog/VHDL source code into a netlist of gates. The software equivalent of synthesis is compilation.

- **.text**/**Text Segment**: See *Code Segment*.

- **TAP**: Test Access Port, a JTAG interface.

- **TCK**: Test Clock. JTAG Clock Signal.
  
- **Tcl**: The defacto standard embedded command language for EDA applications.

- **Udev**: Userspace /dev, a device manager for the Linux kernel.
  
- **USB HID**: USB Human Interface Device class, a part of the USB specification for computer peripherals such as keyboards and mice.

- **VCS**: Version Control Subsystem.

- **Verilate**: To compile HDL to C++ using Verilator.

- **Verilator**: An HDL to C++ compiler.

- **VERA**: Versatile Embedded Retro Adapter, the name of the graphics core used by BoxLambda.

- **VGA**: Video Graphics Array, a computer chipset standard for displaying color graphics.

- **VRAM**: Video RAM.

- **WB**: Wishbone.

- **WIP**: Work In Progress.

- **Wishbone**: An Open-Source SoC bus specification: [https://cdn.opencores.org/downloads/wbspec_b4.pdf](https://cdn.opencores.org/downloads/wbspec_b4.pdf).

- **WSL**: Windows Subsystem for Linux.

- **Xbar**: Cross-Bar, a type of interconnect used in SoC bus fabrics.

- **YM2149**: An '80s era Yamaha sound chip. See also JT49.

