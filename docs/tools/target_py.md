# Target.py

`Target.py` is a target interaction script handling:

- target reset
- flashing/loading bitstream/bootloader/application images
- up/downloading ram disk images
- debugger attachment

`Target.py` is a wrapper around tools such as openocd, OpenFPGAloader and mcopy.

```
usage: target.py [-h] [-verilator] [-flash_bit FLASH_BITSTREAM] [-flash_boot FLASH_BOOT_IMAGE]
                 [-flash_app FLASH_APP_IMAGE] [-load_bit LOAD_BITSTREAM] [-reset] [-load_fs LOAD_FS]
                 [-dump_fs DUMP_FS] [-load_app LOAD_APP_IMAGE] [-run] [-gdb]

Load and control BoxLambda target.

options:
  -h, --help            show this help message and exit
  -verilator            Connect to verilator model instead of FPGA.
  -flash_bit FLASH_BITSTREAM
                        flash bitstream file. Ignored if -verilator flag is set.
  -flash_boot FLASH_BOOT_IMAGE
                        flash bootloader image. Ignored if -verilator flag is set.
  -flash_app FLASH_APP_IMAGE
                        flash application image. Ignored if -verilator flag is set.
  -load_bit LOAD_BITSTREAM
                        Load bitstream file. Ignored if -verilator flag is set.
  -reset                Reset the target before performing other actions.
  -load_fs LOAD_FS      Upload directory as RAM filesystem image.
  -dump_fs DUMP_FS      Dump RAM filesystem image to directory.
  -load_app LOAD_APP_IMAGE
                        Load application image.
  -run                  Execute the application image.
  -gdb                  Wait for GDB to connect.
```

`Target.py` is easy to use directly. It's also used internally by the build system, for instance to implement targets such as `make boxkernfs_load`.

## Example use cases

### Flash boxlambda_base bitstream to target

```
$ cd binaries
$ target.py -flash_bit boxlambda_base.bit
=== Target Control ===
Flashing bitstream file: boxlambda_base.bit
openFPGALoader -b arty_a7_100t -f boxlambda_base.bit
empty
write to flash
Jtag frequency : requested 10.00MHz   -> real 10.00MHz
Open file DONE
Parse file DONE
use: /home/epsilon/work/boxlambda/tools/oss-cad-suite/share/openFPGALoader/spiOverJtag_xc7a100tcsg324.bit.gz
load program
Load SRAM: [==================================================] 100.00%
Done
Shift IR 35
ir: 1 isc_done 1 isc_ena 0 init 1 done 1
JEDEC ID: 0x012018
Detected: spansion S25FL128S 256 sectors size: 128Mb
00000000 00000000 00000000 00
start addr: 00000000, end_addr: 003b0000
Erasing: [==================================================] 100.00%
Done
Writing: [===================================================] 100.00%
Done
(BoxLambda-develop)epsilon@archlin:~/work/boxlambda/binaries$

```

### Flash bootloader to target

```
$ cd binaries
$ target.py -flash_boot bootloader.bin
=== Target Control ===
Flashing bootloader image: bootloader.bin
openFPGALoader -b arty_a7_100t -f -o 5242880 bootloader.bin
empty
write to flash
Jtag frequency : requested 10.00MHz   -> real 10.00MHz
Open file DONE
Parse file DONE
use: /home/epsilon/work/boxlambda/tools/oss-cad-suite/share/openFPGALoader/spiOverJtag_xc7a100tcsg324.bit.gz
load program
Load SRAM: [==================================================] 100.00%
Done
Shift IR 35
ir: 1 isc_done 1 isc_ena 0 init 1 done 1
JEDEC ID: 0x012018
Detected: spansion S25FL128S 256 sectors size: 128Mb
00500000 00000000 00000000 00
start addr: 00500000, end_addr: 00510000
Erasing: [==================================================] 100.00%
Done
Writing: [==================================================] 100.00%
Done
```

### Reset target (booting bitstream and bootloader from flash), load boxkern application image, load forth/fs directory as RAM disk, and run

```
$ target.py -reset -load_app boxkern -load_fs ../../../../../sw/components/forth/fs -run
=== Target Control ===
Reset requested
Uploading dir as RAM disk: ../../../../../sw/components/forth/fs
Loading application image: boxkern
Run requested
dd if=/dev/zero of=boxfs.img bs=1M count=1
1+0 records in
1+0 records out
1048576 bytes (1.0 MB, 1.0 MiB) copied, 0.00149353 s, 702 MB/s
mkfs.fat -S 512 boxfs.img
mkfs.fat 4.2 (2021-01-31)
mcopy -i boxfs.img -s ../../../../../sw/components/forth/fs/forth ../../../../../sw/components/forth/fs/test ::/
openocd -c set RESET 1 -c set LOAD_FS boxfs.img -c set LOAD_APP boxkern -c set RUN 1 -f /home/epsilon/work/boxlambda/scripts/openocd.cfg
Open On-Chip Debugger 0.12.0+dev-00519-gb6ee13720-dirty (2024-11-17-07:21)
Licensed under GNU GPL v2
For bug reports, read
	http://openocd.org/doc/doxygen/bugs.html
1
boxfs.img
boxkern
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
Done.
Loading Filesystem image...
Done.
Loading application image...
Done.
Execute application image...
Done.
(BoxLambda-develop)epsilon@archlin:~/work/boxlambda/build/arty-a7-100/sw/projects/boxlambda_os$
```

### Reset target (booting bitstream and bootloader from flash) and wait for GDB to connect

```
$ target.py -reset -gdb
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
Done.
Waiting for GDB...
Info : Listening on port 6666 for tcl connections
Info : Listening on port 4444 for telnet connections
```

### Connect to a Verilator model

E.g., to debug software running on the model.

```
$ target.py -verilator -gdb
=== Target Control ===
Connection to verilator model requested
Wait for GDB requested
openocd -c set VERILATOR 1 -c set GDB 1 -f /home/epsilon/work/boxlambda/scripts/openocd.cfg
Open On-Chip Debugger 0.12.0+dev-00519-gb6ee13720-dirty (2024-11-17-07:21)
Licensed under GNU GPL v2
For bug reports, read
	http://openocd.org/doc/doxygen/bugs.html
1
1
TAP: riscv.cpu
```

### Download the RAM disk from the target

```
$ target.py -dump_fs ramdisk
=== Target Control ===
Dumping RAM disk to dir: ramdisk
rm -f ramdisk.img
rm -rf ramdisk
openocd -c set DUMP_FS ramdisk.img -f /home/epsilon/work/boxlambda/scripts/openocd.cfg
Open On-Chip Debugger 0.12.0+dev-00519-gb6ee13720-dirty (2024-11-17-07:21)
Licensed under GNU GPL v2
For bug reports, read
	http://openocd.org/doc/doxygen/bugs.html
ramdisk.img
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
Dumping Filesystem image...
Done.
mkdir ramdisk
mcopy -i ramdisk.img -s ::* ramdisk

$ ls -R ramdisk
ramdisk:
forth  test

ramdisk/forth:
boxkern-includes.fs  dump.fs    fixpt-math-lib.fs  ifdef.fs  lambda.fs  struct.fs      utils.fs
cstr.fs              early.fs   fs.fs              init.fs   pool.fs    temp-alloc.fs
dict.fs              escstr.fs  fs-redirect.fs     irq.fs    printf.fs  tonumber.fs
disasm.fs            except.fs  heap.fs            istr.fs   shell.fs   trace.fs

ramdisk/test:
cat.log    chmod.log       cpdir       fph-ext.fs  mount.log  rm.log      testincinc.fs
chdrv.log  core-compat.fs  escstr.log  ls.log      mv.log     testinc.fs  testsuite.fs

ramdisk/test/cpdir:
cpfile0  cpfile1

```

### Upload a RAM disk to the target

```
$ ls -R ramdisk
ramdisk:
forth  test

ramdisk/forth:
boxkern-includes.fs  dump.fs    fixpt-math-lib.fs  ifdef.fs  lambda.fs  struct.fs      utils.fs
cstr.fs              early.fs   fs.fs              init.fs   pool.fs    temp-alloc.fs
dict.fs              escstr.fs  fs-redirect.fs     irq.fs    printf.fs  tonumber.fs
disasm.fs            except.fs  heap.fs            istr.fs   shell.fs   trace.fs

ramdisk/test:
cat.log    chmod.log       cpdir       fph-ext.fs  mount.log  rm.log      testincinc.fs
chdrv.log  core-compat.fs  escstr.log  ls.log      mv.log     testinc.fs  testsuite.fs

ramdisk/test/cpdir:
cpfile0  cpfile1

$ target.py -load_fs ramdisk
=== Target Control ===
Uploading dir as RAM disk: ramdisk
dd if=/dev/zero of=boxfs.img bs=1M count=1
1+0 records in
1+0 records out
1048576 bytes (1.0 MB, 1.0 MiB) copied, 0.00159824 s, 656 MB/s
mkfs.fat -S 512 boxfs.img
mkfs.fat 4.2 (2021-01-31)
mcopy -i boxfs.img -s ramdisk/forth ramdisk/test ::/
openocd -c set LOAD_FS boxfs.img -f /home/epsilon/work/boxlambda/scripts/openocd.cfg
Open On-Chip Debugger 0.12.0+dev-00519-gb6ee13720-dirty (2024-11-17-07:21)
Licensed under GNU GPL v2
For bug reports, read
	http://openocd.org/doc/doxygen/bugs.html
boxfs.img
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
Loading Filesystem image...
Done.
```

