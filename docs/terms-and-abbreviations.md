Terms and Abbreviations
=======================

This section clarifies some of the more ambiguous terms and abbreviations used elsewhere in the documentation.

- **ACK**: A Wishbone Bus Signal.

- **API**: Application Programming Interface.

- **AXI**: Advanced eXtensible Interface, ARM's SoC bus specification.

- **Bitstream**: An FPGA Bitstream is a file containing the programming data associated with an FPGA chip.

- **Blitter**: A type of DMA often used in the context of 2D graphics, copying, combining, and/or modifying bitmap graphics in video memory.

- **BPP**: Bits Per Pixel.

- **BSCANE**: A Xilinx primitive giving access to and from the FPGA's JTAG scan chain.

- **BSS**: Block Starting Symbol. The portion of an object file or executable holding zero-initialized data.

- **Bus Arbiter**: In a Shared Bus Interconnect, a Bus Arbiter decides which of the requesting bus masters gets to access the bus.

- **CDC**: Clock Domain Crossing. The traversal of a signal in a synchronous digital circuit from one clock domain into another.

- **CLI**: Command Line Interface.

- **Clock Domain**: A section of the design driven by one clock, or in some cases, multiple coupled clocks.

- **CMEM**: Code Memory. One of BoxLambda's internal memories. Holds CPU text (instructions) and load segments.

- **CoCoTB**: An open-source coroutine-based cosimulation testbench environment for verifying VHDL and SystemVerilog RTL using Python. See [https://www.cocotb.org/](https://www.cocotb.org/).

- **Code Segment**: Portion of an object file or executable holding executable instructions.

- **Console**: The physical terminal consisting of a screen, a keyboard, and optionally a mouse. Console I/O means input/output from/to these physically attached devices.

- **Constraints File**: A constraints file specifies the mapping of the top-level HDL module's input and output ports to the physical pins of the FPGA. It also defines the clocks used by the given design. See [https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file](https://digilent.com/reference/programmable-logic/guides/vivado-xdc-file).

- **CPP**: C Preprocessor, a macro processor used by the C compiler and the PicoRV *.picoasm* file assembler.

- **CPU**: Central Processing Unit.

- **CRC**: Cyclic Redundancy Check, a checksum algorithm.

- **CRT0**: C Run-Time 0. A set of execution startup routines linked into a C program that performs any initialization work required before calling the program's *main()* function.

- **Crossbar Interconnect**:  An M x N crossbar is a switching fabric that allows M inputs (bus masters) to connect to N outputs (bus slaves) without blocking. Blocking only occurs when two inputs (bus masters) want to talk to the same output (bus slave).

- **CSR**: Control and Status Register.

- **CYC**: A Wishbone Bus Signal.

- **DAC**: Digital-to-Analog Converter.

- **.data**/**Data Segment**: Portion of an object file or executable holding pre-initialized data.

- **DDR PHY**: Double Data Rate (RAM) Physical Interface.

- **DDR SDRAM**: Double Data Rate SDRAM.

- **Delta-Sigma Modulation**: A method used for Digital-to-Analog and Analog-to-Digital conversion. See [https://www.beis.de/Elektronik/DeltaSigma/DeltaSigma.html](https://www.beis.de/Elektronik/DeltaSigma/DeltaSigma.html).

- **DFX**: Dynamic Function Exchange, Xilinx's solution for Partial FPGA Reconfiguration ([https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf](https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2021_2/ug909-vivado-partial-reconfiguration.pdf))

- **DFX Controller**: The DFX Controller IP provides management functions for DFX designs.  When hardware or software trigger events occur, the DFX Controller pulls partial bitstreams from memory and delivers them to an *Internal Configuration Access Port (ICAP)*.  The IP also assists with logical decoupling and startup events, which are customizable per Reconfigurable Partition.

- **DMA**: Direct Memory Access, a hardware assist component offloading memory copy operations from the CPU.

- **DM Reset**: Debug Module Reset signal/domain.

- **DSP**: Digital Signal Processing.

- **DMAC**: DMA Controller.

- **DMEM**: Data Memory. One of BoxLambda's internal memories. Holds CPU data and BSS segments.

- **Double Buffering**:  A technique for drawing graphics that shows no stutter, tearing, or other rendering artifacts. Buffer A is being displayed while buffer B is being updated.

- **DPRAM**: Dual-Port RAM.

- **DTM**: Debug Transport Module.

- **DUT**: Device Under Test.

- **EDA tool**: A software tool to design electronic circuits, e.g. Vivado.

- **EMEM**: External Memory (SDRAM).

- **ERR**: A Wishbone Bus Signal.

- **FFT**: Fast Fourier Transform, a technique used to convert a digital signal into its frequency components.

- **FHDL**: Fragmented Hardware Description Language, a Python-based HDL. Used by Migen and LiteX.

- **FIFO**: First-In-First-out, an implementation of a queue.

- **Fork**: A GitHub fork is a copy of a repository sitting in your account rather than the account from which you forked the data.

- **Forth**: A stack-based programming language.

- **FTDI**: Future Technology Devices International Inc. The name has become synonymous with the USB-to-UART adapter ICs sold by this company.

- **FSM**: Finite State Machine, a synchronous sequential circuit with a finite number of states.

- **Gateware**: [Gateware comprises the description (of behavior, structure and/or connections) of digital logic gates, a high-level abstraction thereof, and/or the implementation thereof in (re)configurable logic devices (such as FPGAs and ASICs)](https://www.gateware.org/definition-of-gateware).

- **GCC**: GNU Compiler Collection. An optimizing compiler produced by the GNU Project.

- **GPIO**: General-Purpose Input/Output, an uncommitted pin used for input and/or output controllable by the user at run-time.

- **Harvard Architecture**: A computer architecture with separate memories and buses for instructions and data. BoxLambda's main CPU, the Ibex processor, uses a Harvard Architecture.

- **Hacker/Hacking**: See [http://www.paulgraham.com/gba.html](http://www.paulgraham.com/gba.html)

- **HAL**: Hardware Access Layer. A low-level Software API to access the hardware, or in this case, gateware.

- **HIR**: Host Interface Registers. The register interface the DMA core presents to the host processor.

- **I2C**: Inter-Integrated-circuit. A bidirectional two-wire synchronous serial bus requiring only two wires to transmit information between devices connected to the bus.

- **I2C Slave Address**: An I2C bus supports multiple slave devices. Each slave device has a unique address, distinguishing it from the other slaves on the bus. This address is what I'm calling the *Slave Address*.

- **I2C Slave Register Address**: Most I2C slave devices implement multiple byte-wide configuration or status registers. These registers are addressed using the first byte of an I2C data frame (i.e. the first byte following the address frame). This address is called the *Slave Register Address*.

- **I2S**: A serial bus interface specially designed for communicating digital audio data between integrated circuits (ICs). The I2S protocol sends pulse-code modulation (PCM) audio data from a controller to a target.

- **Ibex**: The name of the RISC-V CPU core used by BoxLambda.

- **IC**: Integrated Circuit.

- **ICAP**: Internal Configuration Access Port, a Vivado primitive giving access to the FPGA configuration functionality built into Xilinx-AMD FPGAs.

- **Icarus**: Open-Source Verilog simulator. See [https://steveicarus.github.io/iverilog/](https://steveicarus.github.io/iverilog/). Used as the behind-the-scenes simulator when running CoCoTB.

- **IEN**: Interrupt Enable Register.

- **I/F**: Interface.

- **Interconnect**: Wishbone terminology for the bus fabric.

- **IO**: Input/Output.

- **IOB**: Input/Output Block, the part of the FPGA fabric that connects to an IO port (Xilinx terminology. See [Arty A7 Reference Manual](https://digilent.com/reference/programmable-logic/arty-a7/reference-manual)).

- **IP-XACT**: An XML format that defines and describes individual, re-usable electronic circuit designs to facilitate their use in creating integrated circuits.

- **IP Package**: A Vivado file encapsulating an IP component using the IP-XACT file format.

- **IRQ**: Interrupt Request.

- **ICAP**: Internal Configuration Access Port, a Vivado primitive giving access to the FPGA configuration functionality built into AMD-Xilinx FPGAs ([https://www.xilinx.com/products/intellectual-property/axi_hwicap.html](https://www.xilinx.com/products/intellectual-property/axi_hwicap.html))

- **ISA**: Instruction Set Architecture. The Instruction Set Architecture is the part of the processor that is visible to the programmer.

- **ISR**: Interrupt Service Routine or Interrupt Status Register depending on the context.

- **J1**: A small Forth CPU core.

- **JTAG**: Joint Test Action Group, a standard designed to assist with device, board, and system testing, diagnosis, and fault isolation. Today JTAG is used as the primary means of accessing sub-blocks of ICs, making it an essential mechanism for debugging embedded systems.

- **JTAG DTM**: JTAG based Debug Transport Module.

- **JT49**: The name of Jotego's YM2149 compatible sound core implementation.

- **Linting**: Static Code Analysis.

- **LiteDRAM**: A small-footprint and configurable DRAM core. Part of LiteX.

- **LiteX**: A Migen and Python-based SoC Builder framework.

- **LSB**: Least Significant Bit.

- **LUT**: Look-Up Table.

- **Machine Mode**: One of the four RISC-V privilege levels. It's the only mode used in BoxLambda.

- **MCP**: Multi-Cycle Path, a technique for safely passing multiple CDC signals.

- **Makefile**: A file used by the *Make* utility, defining a set of tasks to be executed, and defining dependencies between tasks. Makefiles are commonly used to create build systems.

- **Memory File**: A file containing the initial contents of a Block RAM instance used in an FPGA design.

- **Memory Mapped IO**: Memory-mapped I/O uses the same address space to address both main memory and I/O devices. The memory and registers of the I/O devices are mapped to (associated with) address values. So a memory address may refer to either a portion of physical RAM or instead to the memory and registers of the I/O device.

- **MIG**: Memory Interface Generator, a parameterizable Xilinx IP module used to generate a Memory Controller.

- **MiniEdgeIC**: A 'mini' Edge Triggered Interrupt Controller consisting of an Interrupt Status (ISR) and an Interrupt Enable (IEN) Register.

- **MEMC**: Memory Controller.

- **Migen**: A Python-based toolbox for building digital hardware. Built on FHDL.

- **MMI**: Memory Map Information file. An MMI file is an XML file that syntactically describes how individual block RAMs make up a contiguous logical data space.

- **Mtime**: Machine Timer Register, part of the RISC-V Timer Module.

- **Mtimecmp**: Machine Timer Compare Register, part of the RISC-V Timer Module.

- **MSB**: Most Significant Bit.

- **MUX**: Multiplexer.

- **NDM Reset**: Non-Debug Module reset signal/domain.

- **Nested Interrupt**: A higher priority interrupt that occurs during the execution of another interrupt service routine.

- **NMI**: Non-Maskable Interrupt.

- **OLOGIC**: A dedicated synchronous block sending data out of the FPGA through the IOB (Xilinx terminology. See [Arty A7 Reference Manual](https://digilent.com/reference/programmable-logic/arty-a7/reference-manual)).

- **OOC**: Vivado's OOC mode or OOC flow lets you synthesize, implement, and analyze design modules in a hierarchical design.

- **OpenOCD**: Open On-Chip Debugger, open-source software that interfaces with a hardware debugger's JTAG port.

- **Partition Pins**: Partition pins are the logical and physical connection between the Static Design and an RP. The Vivado toolchain automatically creates, places, and manages partition pins.

- **Pblock**: A Pblock is a user-defined region in the FPGA floor plan specifying the device resources contained within. In the context of DFX, a Pblock is associated with an RP. It defines the FPGA resources (Block RAM, LUTs,...) allocated to that RP.

- **PCB**: Printed Circuit Board.

- **PCM**: Pulse-Code Modulation. PCM data are digital audio samples.

- **PDM**: Pulse Density Modulation.

- **.picoasm**: PicoRV assembly source code file extension.

- **.picobin**: PicoRV program binary file extension.

- **PicoRV32**: A size-optimized RISC-V CPU, used as a *soft* DMA controller in BoxLambda.

- **PIT**: Programmable Interval Timer.

- **PLL Primitive**: A Phase-Locked-Loop-based clock primitive on FPGA.

- **PMOD**: Peripheral Module Interface, an open standard defined by Digilent for connecting peripheral modules to an FPGA.

- **POR**: Power-On Reset.

- **PSG**: Programmable Sound Generator.

- **PUP**: Pull-Up Pin.

- **PWM**: Pulse Width Modulation.

- **RC Filter**: A simple low-pass filter network consisting of a resistor and a capacitor.

- **Reconfigurable Module (RM)**: An RM is the netlist or HDL description of a module that is implemented within a *Reconfigurable Partition (RP)*. Multiple RMs can exist for a given RP. For example, *vs0_stub* and *vs0_j1b* are two RM variants defined for the RP *boxlambda_soc/GENERATE_VS0_MODULE.vs0_inst*. All RM variants belonging to an RP comply with the same interface defined by the RP.

- **Reconfigurable Partition (RP)**: An RP is a module instance in the FPGA design hierarchy marked as *Reconfigurable*. An RP is the container of an RM. BoxLambda has one RP instance with name *boxlambda_soc/GENERATE_VS0_MODULE.vs0_inst*.

- **REPL**: Read-Eval-Print-Loop. The interactive prompt or language shell.

- **Repo**: (Git) Repository.

- **Reset Domain**: A subdomain of a Clock Domain reset by a specific reset signal, e.g. debug module reset domain, non-debug module reset domain, USB reset domain.

- **RP**: Reconfigurable Partition. Part of Xilinx's DFX solution.

- **RM**: Reconfigurable Module. Part of Xilinx's DFX solution.

- **RTCC**: Real-Time Clock and Calendar.

- **RTL**: Register-Transfer Level, an abstraction of a Digital Design, usually captured using a Hardware Description Language such as Verilog, SystemVerilog, or VHDL.

- **RV32IMCB**: Risc-V 32-bit Processor Variant with Multiplier/Divider, Compressed ISA, and Bit Manipulating Extensions.

- **Scatter-Gather DMA**: DMA data transfers from one non-contiguous memory block to another using a series of smaller contiguous block transfers.

- **Scan Line**: One line in the raster scanning pattern of the VGA display.

- **SCL**: I2C Serial Clock line.

- **SD**: Secure Digital, a proprietary non-volatile flash memory card format developed by the SD Association (SDA) for use in portable devices.

- **SDA**: I2C Serial Data Line.

- **SDL**: Simple DirectMedia Layer is a cross-platform development library that provides low-level access to audio, keyboard, mouse, joystick, and graphics.

- **SDRAM**: Synchronous dynamic random-access memory. A DRAM where the operation of its external pin interface is coordinated by an externally supplied clock signal.

- **Shared Bus Interconnect**: A type of Interconnect where one bus master at a time can access a common bus and connect to one of the bus slaves that are attached to that bus. A Bus Arbiter decides which of the requesting bus masters gets to access the bus.

- **Slice**: The basic logical unit of a Xilinx FPGA.

- **SNDH**: A music file format used on Atari ST.

- **(Software) Image**: Snapshot of computer memory contents stored as a file.

- **SoC**: System-on-a-Chip. A System-on-a-Chip is an integrated circuit that integrates all or most components of a computer or other electronic system.

- **SPI**: Serial Peripheral Interface, a synchronous serial communication interface specification used for short-distance communication.

- **Sprite**:  A computer graphic that may be moved on-screen and otherwise manipulated as a single entity.

- **Static Design**: The static design is the part of the design that does not change during partial reconfiguration. It includes the top-level, and all modules not defined as reconfigurable. The static design is built with static logic and static routing.

- **STB**: Wishbone Strobe bus signal.

- **Stderr**: Standard Error. Name of the file object in the Standard C library associated with the standard error device.

- **Stdin**: Standard Input. Name of the file object in the Standard C library associated with the standard input device.

- **Stdout**: Standard Output. Name of the file object in the Standard C library associated with the standard output device.

- **Stdio**: Standard Input and Output. *Stdio.h* is the Standard C Library header file containing input and output functions such as printf() and scanf().

- **ST-Sound**: A software library used to play YM music files. See [https://github.com/arnaud-carre/StSound](https://github.com/arnaud-carre/StSound).

- **Synthesis**: Synthesis turns a module's Verilog/System Verilog/VHDL source code into a netlist of gates. The software equivalent of synthesis is compilation.

- **.text**/**Text Segment**: See *Code Segment*.

- **TAP**: Test Access Port, a JTAG interface.

- **TCK**: Test Clock. JTAG Clock Signal.

- **Tcl**: The defacto standard embedded command language for EDA applications.

- **Udev**: Userspace /dev, a device manager for the Linux kernel.

- **UKP**: A tiny 5-bit processor used in the usb_hid_host core.

- **USB HID**: USB Human Interface Device class, a part of the USB specification for computer peripherals such as keyboards and mice.

- **USB HIDBP**: USB HID Boot Protocol.

- **VCS**: Version Control Subsystem.

- **Vectored Mode**: a RISC-V interrupt handling mode. In vectored mode, each interrupt has a separate entry point in a vector table.

- **Verilate**: To compile HDL to C++ using Verilator.

- **Verilator**: An HDL to C++ compiler.

- **VERA**: Versatile Embedded Retro Adapter, the name of the graphics core used by BoxLambda.

- **VGA**: Video Graphics Array, a computer chipset standard for displaying color graphics.

- **VM**: Virtual Machine, a virtual environment that functions as a virtual computer system.

- **Von Neumann Architecture**: A computer architecture where the CPU accesses one memory (via one bus) storing both instructions and data. The PicoRV processor in BoxLambda's DMA Controller uses a Von Neumann architecture.

- **VRAM**: Video RAM.

- **WB**: Wishbone.

- **WBM**: Wishbone Bus Master.

- **WBS**: Wishbone Bus Slave.

- **Weak Binding**: A software symbol definition that may be overruled by a non-weak re-definition.

- **WIP**: Work In Progress.

- **Wishbone**: An Open-Source SoC bus specification: [https://cdn.opencores.org/downloads/wbspec_b4.pdf](https://cdn.opencores.org/downloads/wbspec_b4.pdf).

- **WSL**: Windows Subsystem for Linux.

- **Xbar**: Cross-Bar, a type of interconnect used in SoC bus fabrics.

- **XPM**: Xilinx Parameterized Macro.

- **YM**: A music file format for the YM2149 chip. See [http://leonard.oxg.free.fr/ymformat.html](http://leonard.oxg.free.fr/ymformat.html).

- **YM2149**: An '80s era Yamaha sound chip. See also JT49.

