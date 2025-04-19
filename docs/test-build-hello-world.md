---
hide:
  - toc
---

## (Debugging) Hello World

### Hello World on the Arty A7

To build the *Hello World!* example, go through the following steps:

Build the project:
```
cd build/arty-a7-100/gw/projects/hello_world
make hello_world_bit
```
Download the bitstream to the target:
```
make hello_world_load
```

#### Connecting GDB to the Hello World build on Arty A7

Verify that the *Hello World* test program is running: The four LEDs on the Arty A7 should be blinking simultaneously.

Start OpenOCD with the *arty_a7_100t.openocd.cfg* config file.

Note: If OpenOCD can't connect to the USB JTAG adapter, your USB device permissions might not be set correctly. Check the *User-Level Access to the Arty A7 USB JTAG Adapter* section above for a fix.
```
openocd -f <boxlambda root directory>/scripts/arty_a7_100t.openocd.cfg

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
Launch gdb-multiarch with the hello_world_ram executable:
```
cd <boxlambda root directory>/build/arty-a7-100/sw/projects/hello_world
gdb-multiarch hello_world_ram
```
Set the architecture to *riscv:rv32* and connect GDB to the target. From the GDB shell:
```
(gdb) set architecture riscv:rv32
The target architecture is set to "riscv:rv32".
(gdb) target extended-remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
Notice that the CPU is stopped at the very first instruction of the boot sequence.

### Hello World Verilator Build

To try out the Verilator Test Bench for *Hello World*:

Build the testbench:
```
cd build/sim-a7-100/gw/projects/hello_world
make hello_world_sim
```
Execute the testbench, with (```./Vmodel -i```) or without (```./Vmodel -t```) tracing:
```
./Vmodel -i/-t
```
View the generated traces:
```
gtkwave simx.fst
```

#### Connecting GDB to the Hello World build on Verilator

Launch the Verilator model with the *-d* flag to indicate that a debugger will be attached to the simulated processor:
```
./Vmodel -d
```
Start OpenOCD with the *verilator.openocd.cfg* config file:
```
openocd -f <boxlambda root directory>/scripts/verilator.openocd.cfg
Open On-Chip Debugger 0.11.0+dev-02372-g52177592f (2022-08-10-14:11)
Licensed under GNU GPL v2
For bug reports, read
		http://openocd.org/doc/doxygen/bugs.html
TAP: riscv.cpu
[riscv.cpu] Target successfully examined.
Ready for Remote Connections on port 3333.
```
Launch gdb-multiarch with the hello_world_ram executable:
```
cd <boxlambda root directory>/build/sim-a7-100/sw/projects/hello_world
gdb-multiarch hello_world_ram
```
Set the architecture to *riscv:rv32* and connect GDB to the target. From the GDB shell:
```
(gdb) set architecture riscv:rv32
The target architecture is set to "riscv:rv32".
(gdb) target extended-remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
Notice that the CPU is stopped at the very first instruction of the boot sequence.

##### *Ignoring Packet Error, Continuing...*

When GDB is connected to a Verilator target, you might occasionally get an annoying *'Ignoring packet error, continuing...'* message in the GDB console. This happens because GDB interaction with a running Verilator model is slow. You can avoid the message by increasing GDB's `remotetimeout` value. The default value is 2 (seconds). On my system, increasing the value to 10 does the trick. In the GDB console, or your *~/.gdbinit* file, enter the following command:

```
set remotetimeout 10
```


