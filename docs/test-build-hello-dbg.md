## Hello_DBG

### Connecting GDB to the Hello_DBG build on Arty A7

Build and run the test project:
```
cd build/arty-a7-[35|100]/gw/projects/hello_dbg
make hello_dbg_impl
make hello_dbg_load
```

Verify that the *Hello World* test program is running: The four LEDs on the Arty A7 should be blinking simultaneously.

Start OpenOCD with the *digilent_arty_a7.cfg* config file. 
Note: If OpenOCD can't connect to the USB JTAG adapter, your USB device permissions might not be set correctly. Check the *User-Level Access to the Arty A7 USB JTAG Adapter* section above for a fix.
```
openocd -f <boxlambda root directory>/scripts/digilent_arty_a7.cfg
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
cd <boxlambda root directory>/build/arty-a7-[35|100]/sw/projects/hello_world
riscv32-unknown-elf-gdb hello_world
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
cd build/sim/gw/projects/hello_dbg
make hello_dbg_sim
```
Launch the Verilator model with the *-d* flag to indicate that a debugger will be attached to the simulated processor:
```
./Vmodel -d
```
Start OpenOCD with the *verilator_riscv_dbg.cfg* config file:
```
openocd -f <boxlambda root directory>/scripts/verilator_riscv_dbg.cfg
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
cd <boxlambda root directory>/build/sim/sw/projects/hello_world
riscv32-unknown-elf-gdb hello_world
```
Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
Notice that the CPU is stopped at the very first instruction of the boot sequence.

