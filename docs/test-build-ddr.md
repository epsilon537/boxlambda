---
hide:
  - toc
---

## DDR Test

### DDR Test Image on Verilator

Build the test project:
```
cd build/sim-a7-100/gw/projects/ddr_test
make ddr_test_sim_sw
```
Execute the generated verilator model in interactive mode:
```
./Vmodel -i
```
You should see something like this:

```
This is a simulation.
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Switching SDRAM to hardware control.
SDRAM init OK.
Memory Test through port 0:
Memtest at 0x20000000 (32.0KiB)...
  Write: 0x20000000-0x20008000 32.0KiB
   Read: 0x20000000-0x20008000 32.0KiB
Memtest OK
Memory port 0 test successful.
DDR instruction access test:
Successfully executed code from DDR.
Test Successful.
```

### DDR Test Image on Arty A7

Build the test project:
```
cd build/arty-a7-100/gw/projects/ddr_test
make ddr_test_bit_sw
```
Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Run the project:
```
make ddr_test_load
```
Verify the test program's output in the terminal. You should see something like this:

![ddr_test on Arty - Putty Terminal](assets/ddr_test_arty.png)

*DDR Test on Arty A7.*
