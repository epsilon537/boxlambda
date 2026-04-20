# Debugging

## On the Arty A7

I will use the [*hello_world* test program](../soc/test/builds/hello-world.md) as an example:

**Important**

The steps below assume that you have executed the BoxLambda [installation procedure](../installation/installation.md), including
the flashing of the bitstream and the bootloader image. If you haven't done so yet, please do that first.

To build and debug the *Hello World!* example, go through the following steps:

Build the software project:

```
cd build/arty-a7-100/sw/projects/hello_world
make hello_world
```

Reset the target and wait for GDB to connect:

```
target.py -reset -gdb
```

This makes the connection from the host to the target using the onboard USB JTAG interface.
Target.py is a Python wrapper script that acts as a frontend for *openocd* and *openFPGALoader*.

You should see something like this in the linux terminal running *target.py*:

```
=== Target Control ===
Reset requested
Wait for GDB requested
openocd -c set RESET 1 -c set GDB 1 -f /home/epsilon/work/boxlambda/scripts/openocd.cfg
Open On-Chip Debugger 0.12.0+dev-00519-gb6ee13720-dirty (2024-11-17-07:21)
Licensed under GNU GPL v2
For bug reports, read
	http://openocd.org/doc/doxygen/bugs.html
1
1
init...
Info : ftdi: if you experience problems at higher adapter clocks, try the command "ftdi tdo_sample_edge falling"
Info : clock speed 25000 kHz
Info : JTAG tap: riscv.cpu tap/device found: 0x13631093 (mfg: 0x049 (Xilinx), part: 0x3631, ver: 0x1)
Info : datacount=2 progbufsize=8
Info : Examined RISC-V core; found 1 harts
Info :  hart 0: XLEN=32, misa=0x40901104
Info : [riscv.cpu] Examination succeed
Info : starting gdb server for riscv.cpu on 3333
Info : Listening on port 3333 for gdb connections
Resetting target...
Info : JTAG tap: riscv.cpu tap/device found: 0x13631093 (mfg: 0x049 (Xilinx), part: 0x3631, ver: 0x1)
```

### In Case of 'unable to open ftdi device' Error

```
Error: libusb_claim_interface() failed with LIBUSB_ERROR_BUSY Error: unable to open ftdi device with vid 0403, pid 6010, description 'Digilent USB Device', serial '*' at bus location '*'
```

If you run into the above error message, you might still have an openocd or vivado hw_server process running somewhere. Kill it from the linux terminal as follows:

```
pkill -f openocd
pkill -f hw_server
```

Thanks to [W. Shepherd Pitts](https://github.com/wspitts2) for this suggestion.

### Connecting GDB

In another linux terminal, launch gdb with the `hello_world` executable:

```
cd <boxlambda root directory>/build/arty-a7-100/sw/projects/hello_world
gdb hello_world

```

Connect GDB to the target and load the exectuable into memmory. From the GDB shell:

```
(gdb) target extended-remote localhost:3333
0x00000150 in main () at /home/epsilon/work/boxlambda/sw/projects/hello_world/hello.cpp:21
21	int main(void) {
(gdb) load
Loading section .etext, size 0x1b8 lma 0x20000000
Loading section .itext, size 0xf80 lma 0x200001b8
Loading section .idata, size 0x228 lma 0x20001138
Start address 0x20000008, load size 4960
Transfer rate: 345 KB/sec, 1653 bytes/write.
```

Notice that the CPU is stopped at the very first instruction of the executable boot sequence.

### Debugging a different bitstream

The previous steps assume the bitstream has been flashed onto the device. If you want to
load a bitstream and debug a bitstream onto the device, without flashing, replace the `target.py`
command in the instructions above with the following:

```
target.py -bit <bitstream file> -reset -gdb
```

## On Verilator

To debug `hello_world` on verilator:

Build the verilator testbench:

```
cd build/sim-a7-100/gw/projects/hello_world
make hello_world_sim
```

Launch the Verilator model with the `-d` flag to indicate that a debugger will be attached to the simulated processor:

```
./Vmodel -d
```

In another linux terminal, run *target.py* with the `-verilator` and `-gdb` flags to connect openocd to the Verilator model and to wait for a
GDB connection:


```
cd build/sim-a7-100/gw/projects/hello_world
target.py -verilator -gdb
```

In yet another linux terminal, launch GDB with the `hello_world` executable:

```
cd <boxlambda root directory>/build/sim-a7-100/sw/projects/hello_world
gdb hello_world

```

Connect GDB to the target. From the GDB shell:

```
(gdb) target extended-remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```

You don't need to load the executable. It's already loaded into memory by the verilator model.

Notice that the CPU is stopped at the very first instruction of the software boot sequence.

### *Ignoring Packet Error, Continuing...*

When GDB is connected to a Verilator target, you might occasionally get an annoying *'Ignoring packet error, continuing...'* message in the GDB console. This happens because GDB's interaction with a running Verilator model is slow. You can avoid the message by increasing GDB's `remotetimeout` value. The default value is 2 (seconds). On my system, increasing the value to 10 does the trick. In the GDB console, or your `~/.gdbinit` file, enter the following command:

```
set remotetimeout 10
```

