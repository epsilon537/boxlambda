Installation, Configuration and Test Builds
===========================================
Installation
------------

Before you try any of the Test Builds below, you need to set up the repository:

Install the [Prerequisites](../documentation/#prerequisites). 

Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
Switch to the *openocd_loose_ends* tag: 
```
git checkout openocd_loose_ends
```
Get the submodules: 
```
git submodule update --init --recursive
```

User-Level Access to the Arty A7 USB JTAG Adapter.
--------------------------------------------------
OpenOCD might not have permission to access the USB JTAG adapter when run at user-level. To fix this issue, you need to add a rule to **/etc/udev/rules.d**.
Create a file with the name **99-openocd.rules** in the **/etc/udev/rules.d** directory. This file should have the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

On WSL
------
### USBIPD-WIN

For USB device access to work at all on WSL, it's necessary to attach the USB device to WSL (by default, USB ports stay under native Windows control). This is done using **usbipd-win**, which can be installed from this location: 

[https://github.com/dorssel/usbipd-win/releases](https://github.com/dorssel/usbipd-win/releases).

Additional info about connecting USB devices to WSL can be found here: 

[https://learn.microsoft.com/en-us/windows/wsl/connect-usb](https://learn.microsoft.com/en-us/windows/wsl/connect-usb).

For convenience, I created a one-line Windows batch script that attaches the Arty USB JTAG port to WSL: 

*boxlambda/wsl/usb_fwd_to_wsl.bat*:

```
usbipd wsl attach -i 0403:6010 -a
```

#### Udev

On Ubuntu WSL, *udev*, the system service in charge of enforcing device permissions, isn't running by default. To fix this, add the following lines to **/etc/wsl.conf**:

```
[boot]
command="service udev start"
```

Without *udev* running, OpenOCD or Vivado will not have access to the Arty USB JTAG adapter when executed at user-level.

Test Builds
-----------

### Hello World on the Arty A7-35T

Project directory **boxlambda/projects/hello_world/** contains a test SoC build consisting of an Ibex_WB core, 64KB internal memory, a wbuart32 core, a timer, and a GPIO module.

To build the *Hello World!* example, go through the following steps:

Build the project:
```
cd projects/hello_world
make impl
```
Download the bitstream to the target:
```
make run
```

### Hello World Verilator Build

To try out the Verilator Test Bench for *Hello World*:

Build the testbench:
```
cd projects/hello_world
make sim
```
Execute the testbench, with (```./Vmodel -i```) or without (```./Vmodel -t```) tracing:
```
cd generated
./Vmodel -i/-t
```
View the generated traces: 
```
gtkwave simx.fst
```

### Connecting GDB to the Hello_DBG build on Arty A7

If you're running on WSL, check the [On WSL](installation-and-test-builds.md#on-wsl) section above to make sure that the USB JTAG adapter is visible and accessible in the WSL environment.

Build and run the test project:
```
cd projects/hello_dbg
make impl
make run
```

Verify that the *Hello World* test program is running: The four LEDs on the Arty A7 should be blinking simultaneously.

Start OpenOCD with the *digilent_arty_a7.cfg* config file. 
Note: If OpenOCD can't connect to the USB JTAG adapter, your USB device permissions might not be set correctly. Check the *User-Level Access to the Arty A7 USB JTAG Adapter* section above for a fix.
```
openocd -f <boxlambda root directory>/openocd/digilent_arty_a7.cfg
Info : clock speed 1000 kHz
Info : JTAG tap: riscv.cpu tap/device found: 0x0362d093 (mfg: 0x049 (Xilinx), part: 0x362d, ver: 0x0)
Info : [riscv.cpu] datacount=2 progbufsize=8
Info : Examined RISC-V core; found 1 harts
Info :  hart 0: XLEN=32, misa=0x40101106
[riscv.cpu] Target successfully examined.
Info : starting gdb server for riscv.cpu on 3333
Info : Listening on port 3333 for gdb connections
Ready for Remote Connections
Info : Listening on port 6666 for tcl connections
Info : Listening on port 4444 for telnet connections
```
Launch GDB with hello.elf:	
```
cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
riscv32-unknown-elf-gdb hello.elf
```
Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
Notice that the CPU is stopped at the very first instruction of the boot sequence.

### Connecting GDB to the Hello_DBG build on Verilator
Build the test project:
```
cd projects/hello_dbg
make sim
```
Launch the Verilator model with the *-d* flag to indicate that a debugger will be attached to the simulated processor:
```
cd generated
./Vmodel -d
```
Start OpenOCD with the *verilator_riscv_dbg.cfg* config file:
```
openocd -f <boxlambda root directory>/openocd/verilator_riscv_dbg.cfg
Open On-Chip Debugger 0.11.0+dev-02372-g52177592f (2022-08-10-14:11)
Licensed under GNU GPL v2
For bug reports, read
		http://openocd.org/doc/doxygen/bugs.html
TAP: riscv.cpu
[riscv.cpu] Target successfully examined.
Ready for Remote Connections on port 3333.
```
Launch GDB with hello.elf:
```
cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
riscv32-unknown-elf-gdb hello.elf
```
Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
Notice that the CPU is stopped at the very first instruction of the boot sequence.

Prerequisites
-------------

- **Host OS**: Linux or Linux WSL.
- **Vivado ML** Edition V2021.2, Linux version:
  
  [https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2021-1.html](https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2021-1.html)
  
  Make sure you also install your Arty A7 or Nexys A7 board files. Digilent has excellent instructions for installing Vivado and Digilent board files:
  
  [https://digilent.com/reference/vivado/installing-vivado/v2019.2](https://digilent.com/reference/vivado/installing-vivado/v2019.2)

- RISCV Compiler Toolchain **rv32imcb**. This is the cross-compiler for building the code that'll run on the Ibex processor. I'm using the **20220210-1** pre-built binaries from *lowRISC*: 
	[https://github.com/lowRISC/lowrisc-toolchains/releases](https://github.com/lowRISC/lowrisc-toolchains/releases)

  Add the toolchain's *bin/* directory to your *PATH*. E.g.:

```
export RISCV_TOOLCHAIN=$HOME/lowrisc-toolchain-gcc-rv32imcb-20220210-1
export PATH=$PATH:$RISCV_TOOLCHAIN/bin
```

- **GNU Make** version 4.2.1: [https://www.gnu.org/software/make/](https://www.gnu.org/software/make/)
  
  Please make sure make is in your *PATH*.
  
- **Bender** 0.25.2: [https://github.com/pulp-platform/bender](https://github.com/pulp-platform/bender)

  Add bender to your *PATH*.

- **Verilator** 4.216: [https://verilator.org/guide/latest/install.html](https://verilator.org/guide/latest/install.html)

  Add verilator to your *PATH*.

- **Ncurses**: ```sudo apt-get install libncurses5-dev libncursesw5-dev libncursesw5```

- **Gtkwave**: [http://gtkwave.sourceforge.net/](http://gtkwave.sourceforge.net/)

- **RISCV OpenOCD**

  Build RISCV OpenOCD from source:
  
```
git clone https://github.com/riscv/riscv-openocd
cd riscv-openocd
git submodule update --init --recursive
./bootstrap
./configure --prefix=$RISCV --disable-werror --disable-wextra --enable-remote-bitbang --enable-ftdi
make
sudo make install
``` 

  Add the install directory (*/usr/local/bin* in my case) to your PATH.  
  &nbsp;
