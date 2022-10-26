---
layout: post
title: 'OpenOCD: Tying Up Loose Ends.'
comments: true
---

Recap
-----
In my [previous post](https://epsilon537.github.io/boxlambda/hello-debugger/), OpenOCD-based debug support was brought up for the Ibex RISCV core. The debug core implementation is based on [RISCV-dbg](https://github.com/pulp-platform/riscv-dbg).

Since then, having worked a bit with the debugger, I did notice a few shortcomings and opportunities for improvement, which I would like to tie up in this brief post. Specifically:

- The target reset function isn't working. The target does not respond to reset commands. This makes it inconvenient to debug early startup code.
- Verilator builds including the RISCV-dbg component require an OpenOCD connection before simulation can start. If I want to just run a simulation, not a debug session, I have to remove RISCV-dbg from the build.
- OpenOCD, when run at user-level, doesn't have access to the Arty A7 USB JTAG adapter. I have to execute OpenOCD using *sudo openocd*.
- JTAG access to the Arty A7 from WSL (Windows Subsystem for Linux) is possible. OpenOCD is doing it. That means that JTAG access to the Arty A7 must also be possible from Vivado running on WSL. I want to get rid of the workaround where I'm running the Vivado Hardware Manager natively on Windows to get access to the Arty A7.

Target reset
------------
While I can attach to a target just fine, the target does not respond to reset commands, not from the OpenOCD configuration script, nor from the GDB monitor.

Some experimentation with the RISCV-dbg code base trace prints and investigation of waveforms showed that the target was not responding to the *JTAG TRST* signal being asserted. A bit of code reading revealed that this happened (or, more accurately, didn't happen) because I had left the **ndmreset** signal unconnected in the *ibex_soc.sv* top-level. 

*Ndmreset* stands for *Non-Debug-Module-Reset*. It's an output signal of the debug core. It's supposed to reset the entire system, except the debug core itself. So that's what I did. I tied ndmreset to the reset input port of every core, except the debug core. That fixed the problem.

[https://github.com/epsilon537/ibex_wb/commit/7f4720af1646abe898ad245e13d1e9083ffb259a](https://github.com/epsilon537/ibex_wb/commit/7f4720af1646abe898ad245e13d1e9083ffb259a)

A Run-Time Flag for the Verilator Model to indicate that OpenOCD Debug Access is Requested.
-------------------------------------------------------------------------------------------

The RISCV-dbg debug core logic blocks on a socket when run in Verilator. This blocks the entire simulation until a socket connection is made by OpenOCD.
This is inconvenient because it means I have to compile out the RISCV-dbg core if I just wanted to run a simulation without a debug session. Instead of having to decide at build-time, I want to choose at run-time whether or not I want to attach OpenOCD to a simulation.

To fix this issue, I added a *jtag_set_bypass()* function to the *sim_jtag* module. If the bypass is set, the *sim_jtag* socket calls are bypassed: 

```
void jtag_set_bypass(int set_bypass) {
  bypass = set_bypass;
}

int jtag_tick(int port, unsigned char *jtag_TCK, unsigned char *jtag_TMS,
              unsigned char *jtag_TDI, unsigned char *jtag_TRSTn,
              unsigned char jtag_TDO)

{
    if (bypass)
      return 0;
    ...
}
```

I tied the *jtag_set_bypass()* call to a *-d* command line option that can be passed to the verilator model:

```
epsilon@...:/mnt/c/work/boxlambda/projects/hello_dbg/generated$ ./Vmodel -h

Vmodel Usage:
-h: print this help
-t: enable tracing.
-d: attach debugger.
```

If the *-d* flag is specified, the Verilator model waits for OpenOCD to connect before continuing the simulation. 
If the *-d* flag is not given, the Verilator model will execute without waiting for an OpenOCD connection.

User-Level Access to the Arty A7 USB JTAG Adapter.
--------------------------------------------------
OpenOCD access to the USB JTAG adapter works when run as root, but not when run at user-level. This indicates there's a permission problem. A Google search quickly shows that I have to add a rule to **/etc/udev/rules.d** to get user-level access to the Arty USB JTAG adapter. 

I created a file, **/etc/udev/rules.d/99-openocd.rules**, with the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

On a native Linux system, this should do the trick. On WSL however...

On WSL
======
I'm on WSL. After fixing the udev permission and a system reboot, I launch OpenOCD with the configuration file for the Arty and... it still doesn't work. OpenOCD still doesn't have the required permission. Bummer.

It turns out that on Ubuntu WSL, *udev* (user /dev), the system service in charge of enforcing these permissions, isn't running by default. Udev *is* part of the distribution, however, so running the service is just a matter of locating the obscure config file where such things are configured. Another Google search reveals that the file in question is **/etc/wsl.conf**. I add the following two lines to that file:

```
[boot]
command="service udev start"
```

Reboot again, launch OpenOCD again, and... success! Hurrah!

#### USBIPD-WIN

Keep in mind that for USB device access to work at all on WSL, it's necessary to attach the USB device to WSL (by default, USB ports stay under native Windows control). This is done using **usbipd-win**, which can be installed from this location: 

[https://github.com/dorssel/usbipd-win/releases](https://github.com/dorssel/usbipd-win/releases).

Additional info about connecting USB devices to WSL can be found here: 

[https://learn.microsoft.com/en-us/windows/wsl/connect-usb](https://learn.microsoft.com/en-us/windows/wsl/connect-usb).

For convenience, I created a one-line Windows batch script that attaches the Arty USB JTAG port to WSL: 

*\<boxlambda root directory\>/wsl/usb_fwd_to_wsl.bat*:

```
usbipd wsl attach -i 0403:6010 -a
```

Make Run
========
The Vivado Hardware Manager can now directly connect to the Arty, also on WSL, I modified the **make run** implementation to use this method to download the bitsteam to the target. This method is more generally fit for use than the previous *make run* implementation, which relied on connecting to a remote hardware manager by IP address.

Arty A7 Access from Vivado on WSL
---------------------------------
In an [earlier post](https://epsilon537.github.io/boxlambda/git-workflow-and-setup/), I wrote about the trouble I was having connecting to my Arty A7 from Vivado running on WSL.
As you may have guessed, this issue is now resolved. The permission issue discussed in the previous section is also what prevented the Vivado Hardware Manager from accessing the Arty A7 from WSL. With the udev and WSL fixes in place, the Vivado Hardware Manager discovers the USB JTAG adapter just fine. Two birds with one stone!

Other Changes
-------------

Read the Docs
=============

The documentation web page was getting out of hand. One single page without a navigation structure just isn't enough. Unfortunately, that's all the current Jekyll theme supports.
I've been looking for other Jekyll themes that support both blogging and documentation, but I haven't found any. Instead, I settled on [Read the Docs](https://readthedocs.org/) in combination with [MkDocs](https://www.mkdocs.org/). MkDocs is Markdown-based, which makes it easy to move content from the Blog to the documentation.

I moved all documentation over to *Read the Docs* and organized it into sections. I hope you like the result:

[https://boxlambda.readthedocs.io/en/latest/](https://boxlambda.readthedocs.io/en/latest/)

Try It Out
----------

Repository setup
================
   0. Install the [Prerequisites](https://boxlambda.readthedocs.io/en/latest/installation-and-test-builds/#prerequisites). 
   1. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
   3. Switch to the *openocd_loose_ends* tag: 
```
git checkout openocd_loose_ends
```
   4. Get the submodules: 
```
git submodule update --init --recursive
```

Connecting GDB to the Ibex RISCV32 processor on Arty A7
=======================================================
   1. If you're running on WSL, check the *On WSL* and *USBIPD-WIN* sections above to make sure that the USB JTAG adapter is visible in the WSL environment.
   2. Build and run the test project:
```
cd projects/hello_dbg
make impl
make run
```
   3. Verify that the *Hello World* test program is running: The four LEDs on the Arty A7 should be blinking simultaneously.
   4. Start OpenOCD with the *digilent_arty_a7.cfg* config file. 
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
   5. Launch GDB with hello.elf:	
```
cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
riscv32-unknown-elf-gdb hello.elf
```
   6. Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
      Notice that the CPU is stopped at the very first instruction of the boot sequence.
   
Connecting GDB to the Ibex RISCV32 processor on Verilator
=========================================================
   1. Build the test project:
```
cd projects/hello_dbg
make sim
```
   2. Launch the Verilator model with the *-d* flag to indicate that a debugger will be attached to the simulated processor:
```
cd generated
./Vmodel -d
```
   3. Start OpenOCD with the *verilator_riscv_dbg.cfg* config file:
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
   4. Launch GDB with hello.elf:
```
cd <boxlambda root directory>/sub/ibex_wb/soc/fpga/arty-a7-35/sw/examples/hello
riscv32-unknown-elf-gdb hello.elf
```
   5. Connect GDB to the target. From the GDB shell:
```
(gdb) target remote localhost:3333
Remote debugging using localhost:3333
?? () at crt0.S:81
81        jal x0, reset_handler
```
   Notice that the CPU is stopped at the very first instruction of the boot sequence.

Interesting Links
-----------------
[https://www.cnx-software.com/2022/09/28/3d-game-fpga-50x-more-efficient-x86-hardware/](https://www.cnx-software.com/2022/09/28/3d-game-fpga-50x-more-efficient-x86-hardware/): Victor Suarez Rovere and Julian Kemmerer built a raytraced game that can run on an Arty A7 *without* a processor. They are using a C-like HDL combo (*PipelineC* and *CflexHDL*) that can be compiled to PC or VHDL. The Arty A7 is not just capable of running this game, it's 50x better at it, efficiency-wise, than an AMD Ryzen.


