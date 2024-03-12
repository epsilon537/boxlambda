## PicoRV DMA Test Builds

### PicoRV Burst FSM CoCoTB Unit Test

[Picorv_burst_fsm_test.py](https://github.com/epsilon537/boxlambda/blob/master/gw/components/picorv_dma/test/picorv_burst_fsm_test.py) is a CoCoTB test script testing the PicoRV Burst FSM module in isolation.

To run the test, navigate to the *picorv_dma* component directory in the build tree and execute the **picorv_burst_fsm_cocotb_test** test case, using ctest:
```
cd build/sim-a7-100/gw/components/picorv_dma
ctest -V -R picorv_burst_fsm_cocotb_test
...<a lot of Wishbone transaction traces etc.>...
1/1 Test #2: picorv_burst_fsm_cocotb_test .....   Passed    1.80 sec
The following tests passed:
        picorv_burst_fsm_cocotb_test
100% tests passed, 0 tests failed out of 1
Total Test time (real) =   1.82 sec
```

### PicoRV DMA CocoTB Unit Test

[Picorv_dma_test.py](https://github.com/epsilon537/boxlambda/blob/master/gw/components/picorv_dma/test/picorv_dma_test.py) is a CoCoTB test script testing the complete PicoRV DMA core in isolation.

To run the test, navigate to the *picorv_dma* component directory in the build tree and make the *picorv_dma_test* target to build all the PicoRV assembly programs the unit test depends on:
```
cd build/sim-a7-100/gw/components/picorv_dma
make picorv_dma_test
```
Execute the **picorv_dma_cocotb_test** test case using ctest:
```
ctest -V -R picorv_dma_cocotb_test
...<a lot of Wishbone transaction traces etc.>...
1/1 Test #1: picorv_dma_cocotb_test ...........   Passed   25.67 sec
The following tests passed:
        picorv_dma_cocotb_test
100% tests passed, 0 tests failed out of 1
Total Test time (real) =  25.67 sec
```

### PicoRV DMA Extended System Test on Verilator

The PicoRV DMA Extended System Test consists of multiple nested loops iterating over all permutations of a series of test vectors:

- Source Memory Type: Internal, External, VRAM.
- Destination Memory Type: Internal, External, VRAM.
- PicoRV Program: Byte Copy Single, Byte Copy Burst, Word Copy Single, Word Copy Burst.
- Source Pointer byte offset 0..3.
- Destination Pointer byte offset 0..3.
- Number of bytes/words to copy.

At the center of the nested loop is a parameterized DMA copy routine followed by checks verifying if the DMA copy is complete, without errors and out-of-bounds writes.

To build the project:
```
cd build/sim-a7-100/gw/projects/picorv_dma_sys_test_ext
make picorv_dma_sys_test_ext_sim_sw
```
Execute the generated Verilator model. You should see the following output:
```
./Vmodel
...
Initializing SDRAM @0x40000000...
Switching SDRAM to software control.
Switching SDRAM to hardware control.
SDRAM init OK.
Load PicoRV Program WORDCOPY_BURST
Taking PicoRV out of reset...
Src Mem Type LOCAL, addr=0x84c4
Dst Mem Type LOCAL, addr=0x83c4
Dst Mem Type EXT_MEM_1, addr=0x50000100
Dst Mem Type LOCAL, addr=0x83c4
...
Load PicoRV Program BYTECOPY_BURST
Taking PicoRV out of reset...
Src Mem Type LOCAL, addr=0x84c4
Dst Mem Type LOCAL, addr=0x83c4
Dst Mem Type EXT_MEM_1, addr=0x50000100
...
Putting PicoRV back into reset...
All tests passed.
Test passed.
```

### PicoRV DMA Extended System Test on Arty A7

If you're running on WSL, check the [On WSL](https://boxlambda.readthedocs.io/en/latest/installation/#on-wsl) section.
Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.
Build the project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):
```
cd build/arty-a7-100/gw/projects/picorv_dma_sys_test_ext
make picorv_dma_sys_test_ext_bit_sw
```
Download the generated bitstream file to the Arty A7:
```
make picorv_dma_sys_test_ext_load
```
In the Putty terminal, you should see the same output as with the Verilator test build above.
