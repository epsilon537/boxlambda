## Reset Test

The reset test code running on the RISCV processor: [boxlambda/sw/projects/reset_test/reset_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/reset_test/reset_test.cpp)

The Verilator test bench code: [boxlambda/gw/projects/reset_test/sim/sim_main.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/reset_test/sim/sim_main.cpp)

The reset test checks the software interaction with BoxLambda's reset controller. The test does the following:

1. Retrieve the reset reason after reset (POR, External Reset, SW triggered reset...).
2. Trigger a system reset via software.

### Reset Test on Verilator

Build the test project:
```
cd build/sim-a7-100/gw/projects/reset_test
make reset_test_sim_sw
```
Execute the generated verilator model:
```
./Vmodel
```
You should see something like this:

```
DUT: Starting...
DUT: Initializing SDRAM @0x20000000...
DUT: Switching SDRAM to software control.
DUT: Switching SDRAM to hardware control.
DUT: SDRAM init OK.
DUT: Reset Reason: Power-On Reset.
SIM: String matched. Moving on...
DUT: Push btn[0] to SW trigger DM+NDM Reset.
SIM: String matched. Moving on...
SIM: Pushing button 0...
SIM: NDM and DM reset trigger detected.
SIM: Releasing button 0...
DUT: SW triggering DStarting...
DUT: Initializing SDRAM @0x20000000...
DUT: Switching SDRAM to software control.
DUT: Switching SDRAM to hardware control.
DUT: SDRAM init OK.
DUT: Reset Reason: SW triggered Non-Debug Module Reset.
SIM: String matched. Moving on...
DUT: Reset Reason: SW triggered Debug Module Reset.
SIM: String matched. Moving on...
SIM: Asserting external reset...
DUT: Push btn[0] to SW trigger DM+NDM Reset.
SIM: Releasing external reset...
DUT: Starting...
DUT: Initializing SDRAM @0x20000000...
DUT: Switching SDRAM to software control.
DUT: Switching SDRAM to hardware control.
DUT: SDRAM init OK.
DUT: Reset Reason: External Reset.
SIM: String matched. Moving on...
SIM: Test passed.
```

### Reset Test Image on FPGA

Build the test project:
```
cd build/arty-a7-[35|100]/gw/projects/reset_test
make reset_test_bit_sw
```
Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.

Run the project:
```
make reset_test_load
```
Verify that software reports Power-On Reset as the reset reason:
```
Starting...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
...
Switching SDRAM to hardware control.
SDRAM init OK.
Reset Reason: Power-On Reset.
String matched. Moving on...
Push btn[0] to SW trigger DM+NDM Reset.
```
Press button 0, which should cause software to issue a Debug Module and Non-Debug Module reset request.
The system should restart and software should report a software-triggered Non-Debug Module and Debug
Module reset reason:
```
SW triggering DStarting...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
...
Switching SDRAM to hardware control.
SDRAM init OK.
Reset Reason: SW triggered Non-Debug Module Reset.
Reset Reason: SW triggered Debug Module Reset.
Push btn[0] to SW trigger DM+NDM Reset.
```
Press the external reset button. The system should restart and software should report an *External Reset*
reason:
```
Starting...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
...
Switching SDRAM to hardware control.
SDRAM init OK.
Reset Reason: External Reset.
Push btn[0] to SW trigger DM+NDM Reset.
```

