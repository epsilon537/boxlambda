---
hide:
  - toc
---

## Real-Time Clock and Calendar Test

- [boxlambda/sw/projects/rtcc_test/rtcc_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_test.cpp)
- [boxlambda/sw/projects/rtcc_test/rtcc_cli.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_cli.cpp)

The Real-Time Clock and Calendar test program does the following:

1. Boot from Flash Memory.
2. Every second, retrieve the current time from the RTCC PMOD via I2C.
3. Drop into a CLI when button 0 is pressed.
4. From the CLI, the user can set the time, get the time, set the date, and get the date.

This test program does *not* run on Verilator.

### The RTCC Test on FPGA

Hook up the RTCC PMOD as described [here](pmods.md#rtcc-pmod).

Connect a terminal emulator to Arty's USB serial port. **Settings: 115200 8N1**.

Build the `rtcc_test_flsh` software project in an Arty A7 build tree:
```
cd build/arty-a7-100/sw/projects/rtcc_test
make rtcc_test_flsh
```
Flash the `rtcc_test_flsh` program onto the target:
```
make rtcc_test_flsh_flash_sw
```
Build the rtcc_test gateware project in an Arty A7 build tree:
```
cd build/arty-a7-100/gw/projects/rtcc_test
make rtcc_test_bit
```
Flash the gateware build onto the target:
```
make rtcc_test_flash_gw
```
When flashing has been completed, the target will be reset. In the terminal, you should now see the following output:

```
Delaying start by 1s...
Starting...
Initializing SDRAM @0x20000000...
...
SDRAM init OK.
RTC oscillator is not running yet. Enabling...
Polling time in a loop. Push btn[0] to end loop and start CLI.
sec: 0x1, min: 0x2, hour: 0x3.
sec: 0x2, min: 0x2, hour: 0x3.
sec: 0x3, min: 0x2, hour: 0x3.
sec: 0x4, min: 0x2, hour: 0x3.
sec: 0x5, min: 0x2, hour: 0x3.
sec: 0x6, min: 0x2, hour: 0x3.

```

Power down the Arty A7. Wait a while, then power it back up.

You should see time running with the powered-down time gap accounted for. After power-up time should **not** be reset back to 0x1:0x2:0x3 :

```
RTC oscillator is already running.
Polling time in a loop. Push btn[0] to end loop and start CLI.
sec: 0x1f, min: 0x22, hour: 0x3.
sec: 0x20, min: 0x22, hour: 0x3.
sec: 0x21, min: 0x22, hour: 0x3.
sec: 0x22, min: 0x22, hour: 0x3.
sec: 0x23, min: 0x22, hour: 0x3.
```

Press button 0 to drop into the CLI, then type 'help' to see a list of available commands:

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
 * settime
        settime <hour> <minute> <second>, e.g. 16 56 00
 * gettime
        Get Current Time
 * setdate
        setdate <day> <month> <year> e.g. 23 7 2024
 * getdate
        Get Current Date
```

Play around with the settime/gettime/setdate/getdate commands. For example:

```
> setdate 7 8 2024
Done.
> getdate
The current date is (dd/mm/yyyy): 07/08/2024.
>
```

