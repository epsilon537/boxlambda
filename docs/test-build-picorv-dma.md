## PicoRV DMA Test Builds

### PicoRV DMA CocoTB Unit Test

Navigate to the *picorv_dma* component directory in the build tree and make the *picorv_dma_test* target to build all the PicoRV assembly programs the unit test depends on:
```
cd build/sim-a7-100/gw/components/picorv_dma
make picorv_dma_test
```
Execute the CocoTB unit test program by running ctest in the current directory. You should see the following output:
```
ctest
Test project /home/epsilon/work/boxlambda/build/sim-a7-100/gw/components/picorv_dma
Start 1: picorv_dma_cocotb_test
1/1 Test #1: picorv_dma_cocotb_test ...........   Passed    9.04 sec
100% tests passed, 0 tests failed out of 1
Total Test time (real) =   9.06 sec
```
If you find this a little underwhelming, you can run ctest again with the *-V* verbose flag to see more details.

### PicoRV DMA System Test on Verilator

Build the *picorv_dma_sys_test* project:
```
cd build/sim-a7-100/gw/projects/picorv_dma_sys_test
make picorv_dma_sys_test_sim_sw
```
Execute the generated Verilator model. You should see the following output:
```
./Vmodel
...
Initializing SDRAM @0x40000000...
Switching SDRAM to software control.
Switching SDRAM to hardware control.
SDRAM init OK.
Load PicoRV WordCopy Program...
Taking PicoRV out of reset...
Internal memory wordcopy test
Configuring DMA request.
numElems = 32, srcAddr = 0x80e4, dstAddr = 0x8164
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Wordcopy test successful.
External memory wordcopy test, port 0
Configuring DMA request.
numElems = 32, srcAddr = 0x40000000, dstAddr = 0x40000400
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Wordcopy test successful.
External memory wordcopy test, port 1
Configuring DMA request.
numElems = 32, srcAddr = 0x50000000, dstAddr = 0x50000400
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Wordcopy test successful.
External memory wordcopy to VRAM test
Configuring DMA request.
numElems = 32, srcAddr = 0x40000000, dstAddr = 0x10140000
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Wordcopy test successful.
Putting PicoRV back into reset...
Load PicoRV ByteCopy Program...
Taking PicoRV out of reset...
Internal memory bytecopy test
Configuring DMA request.
numElems = 32, srcAddr = 0x81e5, dstAddr = 0x820b
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Bytecopy test successful.
External memory bytecopy test, port 0.
Configuring DMA request.
numElems = 32, srcAddr = 0x40000001, dstAddr = 0x40000403
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Bytecopy test successful.
External memory bytecopy test, port 1.
Configuring DMA request.
numElems = 32, srcAddr = 0x50000001, dstAddr = 0x50000403
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Bytecopy test successful.
External memory bytecopy to VRAM test
Configuring DMA request.
numElems = 32, srcAddr = 0x40000001, dstAddr = 0x10140003
Kicking off DMA...
Waiting for completion...
Checking result...
PicoRV Bytecopy test successful.
PicoRV DMA tests successful.
Test passed.
```

### PicoRV DMA System Test on Arty A7

If you're running on WSL, check BoxLambda's documentation [On WSL](https://boxlambda.readthedocs.io/en/latest/installation/#on-wsl) section.

Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.

Build the *picorv_dma_sys_test* project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
```
cd build/arty-a7-100/gw/projects/picorv_dma_sys_test
make picorv_dma_sys_test_bit_sw
```
Download the generated bitstream file to the Arty A7:
```
make picorv_dma_sys_test_load
```
In the Putty terminal, you should see the same output as with the Verilator test build above.
