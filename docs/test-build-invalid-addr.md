## Invalid Address Test

This test verifies that reading from or writing to an invalid address does not cause an exception and that reading from an invalid address returns the value 0xDEADBEEF.

### Invalid Address Test on Verilator

Build the **invalid_address** project:

```
cd build/sim-a7-100/gw/projects/invalid_address
make invalid_address_sim_sw
```

Execute the generated Verilator model. You should see the following output:

```
./Vmodel
DUT: This is a simulation.
DUT: Writing 0 to invalid address 0x18000000:
DUT: Reading from invalid address 0x18000000:
DUT: 0x18000000: 0xdeadbeef
DUT: Writing 0 to invalid address 0x10080000:
DUT: Reading from invalid address 0x10080000:
DUT: 0x10080000: 0xdeadbeef
DUT: Writing 0 to invalid address 0x20000000:
DUT: Reading from invalid address 0x20000000:
DUT: 0x20000000: 0xdeadbeef
DUT: Writing 0 to invalid address 0x10000400:
DUT: Reading from invalid address 0x10000400:
DUT: 0x10000400: 0xdeadbeef
DUT: Test Successful.
SIM: Test passed.
```

### Invalid Address Test on Arty A7

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Build the project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/invalid_address
make invalid_address_bit_sw
```

Download the generated bitstream file to the Arty A7:

```
make invalid_address_load
```

In the Putty terminal, you should see the same output as in the Verilator test build above.
