## I2C Test

The I2C test code running on the RISCV processor: [boxlambda/sw/projects/rtcc_test/i2c_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/i2c_test/i2c_test.cpp)

The Verilator test bench code: [boxlambda/gw/projects/i2c_test/sim/sim_main.cpp](https://github.com/epsilon537/boxlambda/blob/master/gw/projects/i2c_test/sim/sim_main.cpp)

The Verilator version of the I2C Test Application uses an I2C slave co-simulator object provided by the WBI2C repo:

- [boxlambda/sub/wbi2c/bench/cpp/i2csim.h](https://github.com/epsilon537/wbi2c/blob/boxlambda/bench/cpp/i2csim.h)
- [boxlambda/sub/wbi2c/bench/cpp/i2csim.cpp](https://github.com/epsilon537/wbi2c/blob/boxlambda/bench/cpp/i2csim.cpp)

The I2C System Test Case runs on Verilator and FPGA, albeit with different test features. The program does the following:

1. On Verilator it reads the initial contents of a co-simulated I2C slave memory and checks the fetched values against the expected contents. If there were an endianness issue in the I2C path (which may happen with ZipCPU-based cores because ZipCPU is big-endian and BoxLambda is little-endian), this test would catch it.
2. It writes a test string into the I2C slave memory.
3. It reads back the test string from the I2C slave memory.
4. It verifies that WBI2C generates an IRQ when an I2C transaction has been completed.
5. On FPGA, you get dropped into a CLI built on top of the I2C API. From this CLI you can issue your own I2C transactions.

### The I2C Test on Verilator

Build the **i2c_test** project:

```
cd build/sim-a7-100/gw/projects/i2c_test
make i2c_test_sim_sw
```

Execute the generated Verilator model. You should see the following output:

```
./Vmodel
DUT: Initializing SDRAM @0x20000000...
DUT: Switching SDRAM to software control.
DUT: Switching SDRAM to hardware control.
DUT: SDRAM init OK.
DUT: Enabling IRQs
DUT: This is a simulation. Setting simulation I2C clock.
DUT: Reading slave registers' initial contents (endianness test).
DUT: OK.
DUT: Sending test string to I2C slave.
DUT: Reading back from slave.
DUT: Comparing strings.
DUT: OK.
DUT: I2C IRQS received: 3
DUT: OK.
DUT: Test Successful.
DUT: Push btn[0] to start CLI.
```

### The I2C Test on FPGA

Hook up the RTCC PMOD as described [here](pmods.md#rtcc-pmod).

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 115200 8N1**.

Build the project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/i2c_test
make i2c_test_bit_sw
```

Download the generated bitstream file to the Arty A7:

```
make i2c_test_load
```

You should see the following output in the terminal:

```
Initializing SDRAM @0x20000000...
...
SDRAM init OK.
Enabling IRQs
This is not a simulation. Setting 100kHz I2C clock.
Sending test string to I2C slave.
Reading back from slave.
Comparing strings.
OK.
I2C IRQS received: 2
OK.
Test Successful.
Push btn[0] to start CLI.
```

Press button 0 to get dropped into the CLI. Type *help* to see a list of available commands:

```
> help
 * help
        Print list of commands
 * peekw
        Peek word: peekw <hex addr>
 * peekb
        Peek byte: peekb <hex addr>
 * pokew
        Poke word: pokew <hex addr> <hex value>
 * pokeb
        Poke byte: pokeb <hex addr> <hex value>
 * beginTransmission
        I2C beginTransmission <hex slaveAddr>
 * endTransmission
        I2C endTransmission
 * requestFrom
        I2C requestFrom <hex slaveAddr> <num. bytes>
 * i2cread
        i2cread next byte
 * i2cwrite
        i2cwrite <hex byte value>
```

Using the CLI, issue an I2C transaction. For example, read I2C slave register 0:

```
> beginTransmission 6f
beginTransmission 0x6f
> i2cwrite 0
i2cwrite 0x0
> endTransmission
endTransmission OK
> requestFrom 6f 1
requestFrom 0x6f 1 -> OK
> i2cread
i2cread -> 0x92
```

