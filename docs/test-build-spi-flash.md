## SPI Flash Test

The **spiflash_test** program does the following:

1. Boot from Flash Memory.
2. Read the Flash ID.
3. Write a character string to a specific Flash Memory location.
4. Read back the character string from Flash Memory and compare it against the written string.
5. Write a different character string of the same length to the same Flash Memory location.
6. Read back the character string from Flash Memory and compare it against the written string.

### The SPI Flash Test on Verilator

Build the **spiflash_test** gateware project. This will also build the *spiflash_test_flsh.bin* software image as a dependency:
```
cd build/sim-a7-100/gw/projects/spiflash_test
make spiflash_test_sim_sw
```

Execute the generated Verilator model, passing in as a parameter the *spiflash_test_flsh.bin* software image. The software image will be loaded into the simulated flash device before the test starts:
```
./Vmodel -f ../../../sw/projects/spiflash_test/spiflash_test_flsh.bin
Flash SW Image File: ../../../sw/projects/spiflash_test/spiflash_test_flsh.bin
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

### The SPI Flash Test on FPGA

Connect a terminal emulator to Arty's USB serial port. **Settings: 115200 8N1**.

Build the *spiflash_test_flsh* software project in an Arty A7 build tree:
```
cd build/arty-a7-100/sw/projects/spiflash_test
make spiflash_test_flsh
```
Flash the *spiflash_test* program onto the target:
```
make spiflash_test_flsh_flash_sw
```
Build the gateware project in an Arty A7 build tree:
```
cd build/arty-a7-100/gw/projects/spiflash_test
make spiflash_test_bit
```
Flash the gateware build onto the target:
```
make spiflash_test_flash_gw
```
When flashing has been completed, the target will be reset. In the terminal, you should now see the same output as with the Verilator test build above.

