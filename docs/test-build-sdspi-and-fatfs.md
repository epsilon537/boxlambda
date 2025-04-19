---
hide:
  - toc
---

## SDSPI and FatFs Test

### SDSPI Test on Verilator

Build the *sdspi_test* project:

```
cd build/sim-a7-100/gw/projects/sdspi_test
make sdspi_test_sim
```

Create and format the SD Card image file (or just use the *sdcard.img* files checked into the *test/* subdirectory of the *sdspi_test* project):

```
dd if=/dev/zero of=sdcard.img bs=512 count=131072
mkfs.fat -F 16 sdcard.img
```

Execute the generated Verilator model. Pass in the *sdcard.img* file:

```
./Vmodel -s sdcard.img
```

You should now see the following messages appear in the terminal window. The traces prefixed with *SDSPI:* come from the SDSPI co-simulator. The first two lines and the last line come from the test bench. The other lines are *printf()* statements coming from the test program running on the RISCV processor.

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

### SDSPI Test on Arty A7

Hook up the MicroSD PMOD as described [here](pmods.md#microsd-pmod).

Note that this is a destructive test. The contents of the SD card will be destroyed.

Build the *sdspi_test* project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/sdspi_test
make sdspi_test_bit
```

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Download the generated bitstream file to the Arty A7:

```
make sdspi_test_load
```

Verify the test program's output in the terminal. You should see something like this:

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

### FatFS Test on Verilator

Build the *fatfs_test* project:

```
cd build/sim-a7-100/gw/projects/fatfs_test
make fatfs_test_sim
```

Create and format the SD Card image file (or just use the *sdcard.img* files checked into the *test/* subdirectory of the *fatfs_test* project):

```
dd if=/dev/zero of=sdcard.img bs=512 count=131072
mkfs.fat -F 16 sdcard.img
```

   Execute the generated Verilator model. Pass in the *sdcard.img* file:

```
./Vmodel -s sdcard.img
```

You should see the following messages in the terminal window. The traces prefixed with *SDSPI:* come from the SDSPI co-simulator. The first two lines and the last line come from the test bench. The other lines are *printf()* statements coming from the test program running on the RISCV processor.

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

One of the steps taken by the test program is to create a `LOG.TXT` file with the contents *This is a test*. We can mount the *sdcard.img* on Linux and check if that file exists with the expected contents:

```
sudo mount -o loop ~/sdcard.img /mnt/sd
ls -al /mnt/sd
cat /mnt/sd/LOG.TXT
This is a test.
sudo umount /mnt/sd
```

### FatFS Test on Arty A7

Hook up the MicroSD PMOD as described [here](pmods.md#microsd-pmod).

Note that this is a destructive test. The contents of the SD card will be destroyed.

Build the *fatfs_test* project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/fatfs_test
make fatfs_test_bit
```

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Download the generated bitstream file to the Arty A7:

```
make fatfs_test_load
```

Verify the test program's output in the terminal. You should see something like this:

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

One of the steps taken by the test program is to create a `LOG.TXT` file with the contents *This is a test*. Eject the SD card, insert it into your PC, and verify that *LOG.TXT* exists with the expected contents.

&nbsp;
