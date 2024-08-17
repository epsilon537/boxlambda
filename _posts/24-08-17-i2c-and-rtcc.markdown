---
layout: post
title: 'Keeping Time: RTCC and I2C.'
comments: true
mathjax: yes
---

BoxLambda is a computer. A computer should be able to keep the time and date, even when switched off. One way to keep the time and date is by using a *Real-Time Clock and Calendar* (RTCC) module with a coin cell battery backup. The RTCC is an external PMOD component, interfacing with the BoxLambda SoC using I3C.

Recap
-----
![BoxLambda Block Diagram.](../assets/Arch_Diagram_I2C_Focus.png)

This is a summary of the current state of affairs of BoxLambda. We have:
- An Ibex RISC-V core with machine timer and hardware interrupt support.
- Wishbone interconnect and Harvard architecture internal memory.
- DDR3 external memory access through the Litex memory controller.
- OpenOCD-based debug access on FPGA and Verilator.
- VERA-based VGA graphics: 2 layers tile or bitmap mode, 2 banks of 64 sprites, 128KB Video RAM, 256 color palette.
- Dual YM2149 PSG Audio.
- SD Card Controller and FatFs File System.
- 24-pin GPIO, UART, SPI Flash Controller.
- USB HID Keyboard and Mouse support.
- A PicoRV32-based Programmable DMA Controller.
- A Picolibc-based standard C environment for software running on the Ibex RISC-V core.
- A *Big* and a *Little* configuration targeting FPGA development boards Arty-A7-35T and Arty-A7-100T respectively.
- A suite of test applications targeting all SoC components. Each test application runs on both FPGA configurations and on Verilator.
- Automated testing on Verilator and CocoTB.
- A Linux-based CMake and Bender-based Software and Gateware build system.

The BoxLambda SoC top-level: [gw/components/boxlambda_soc/rtl/boxlambda_soc.sv](https://github.com/epsilon537/boxlambda/blob/master/gw/components/boxlambda_soc/rtl/boxlambda_soc.sv)

I2C
---

I won't turn this post into yet another I2C tutorial If you're looking to learn about the I2C protocol in general, please check out the following resources:

- [https://www.robot-electronics.co.uk/i2c-tutorial](https://www.robot-electronics.co.uk/i2c-tutorial)
- [https://learn.sparkfun.com/tutorials/i2c/all](https://learn.sparkfun.com/tutorials/i2c/all)
- [https://ai.thestempedia.com/docs/evive/evive-technical-specifications/arduino-core-interface/i2c-communication/](https://ai.thestempedia.com/docs/evive/evive-technical-specifications/arduino-core-interface/i2c-communication/)
- [https://en.wikipedia.org/wiki/I%C2%B2C](https://en.wikipedia.org/wiki/I%C2%B2C)

WBI2C
=====

Although it's perfectly doable to implement I2C in software using bit banging, I chose to use a gateware-assist. I'm using the I2C Bus Master core of ZipCPU's WBI2C repo. As usual, I forked the repo so I can easily implement customizations for BoxLambda. Here's the GitHub page:

[https://github.com/epsilon537/wbi2c](https://github.com/epsilon537/wbi2c)

This is the I2C Bus Master core top-level:

[https://github.com/epsilon537/wbi2c/blob/boxlambda/rtl/wbi2cmaster.v](https://github.com/epsilon537/wbi2c/blob/boxlambda/rtl/wbi2cmaster.v)

### The Slave Register Protocol

WBI2C implements the traditional I2C protocol used to access 8-bit I2C slave registers. The protocol uses an address frame consisting of a 7-bit slave address and a read/write bit, followed by one or more byte-sized data frames. The first of these data frames contains the slave register address.

![I2C transaction with 7-bit slave address](../assets/I2C-Bits-Protocol.jpg)

*I2C transaction with 7-bit slave address and 8-bit slave register address (Source: STEMpedia).*

10-bit addressing is not supported.

A quick note on terminology:

- **I2C Slave Address**: An I2C bus supports multiple slave devices. Each slave device has a unique address, distinguishing it from the other slaves on the bus. This address is what I'm calling the *Slave Address* in the discussion below.
- **I2C Slave Register Address**: Most I2C slaves implement multiple byte-wide configuration and status registers. These registers are addressed using the first byte of an I2C data frame (i.e. the first byte following the address frame). This address is called the *Slave Register Address*.

### Theory of Operation

![WBI2C Master Core](../assets/wbi2c_master.png)

*The WBI2C Master Core.*

The WBI2C core contains a 256-byte memory acting as a proxy for the register space of the I2C slave device. The idea is that software writes into the proxy memory, at the intended register addresses, and then requests WBI2C to push this data to the I2C slave registers using the I2C Slave Register Protocol. Vice versa when reading from the slave.

Going into a little more detail, to write to a single I2C slave register:

1. Software writes the register byte value into the proxy memory at the slave register address.
2. Software writes to WBI2C's command (CMD) register, specifying:
    - the 7-bit slave address.
    - the 8-bit slave register address (corresponding with a proxy memory address location).
    - the number of bytes to send (1 in case of a single register write).
    - a flag indicating that this is a write transaction.
4. WBI2C reads the register value from proxy memory and sends it to the slave using the Slave Register Protocol.
5. Software waits for the completion of the transaction by polling the CMD register **Busy** bit. Instead of polling, WBI2C can be configured to generate an IRQ when the transaction is complete.

**I2C CMD Register Layout**:

| Bit 31 | Bit 30 | Bits 29:24 | Bits 23:17 | Bit 16 | Bits 15:8 | Bits 7:0 |
|--------|--------|------------|------------|--------|-----------|----------|
| Busy   | Err.    | -         | Slave Addr. | R (1)/ W (0) | Slave Reg. Addr. | Num. Bytes |

To read from a single I2C slave register:

1. Software initiates a read transaction by writing to the WBI2C CMD register, specifying:
    - the 7-bit slave address.
    - the slave register address to read.
    - the number of bytes to read (1 in case of a single register read).
    - a flag indicating that this is a read transaction.
2. WBI2C reads the requested I2C slave register using the Slave Register Protocol and stores the retrieved byte in the proxy memory, at the slave register address.
3. Software waits for the completion of the transaction by polling the CMD register *Busy* bit. Instead of polling, WBI2C can be configured to generate an IRQ when the transaction is complete.
4. Software reads the retrieved I2C register value from the proxy memory, at the slave register address.

#### Multi-Byte Transactions

The 256-byte memory is overkill for reading and writing individual slave registers. The advantage of this memory comes into play when reading or writing a byte *sequence*. Instead of babysitting the entire transaction byte-by-byte, a bulk transaction can be kicked off. The CPU can move on to other tasks while the transaction is ongoing. In a multi-byte transaction, WBI2C automatically increments the slave register address value after each byte sent/retrieved, wrapping around to 0 if the address value goes beyond 255.

For example, let's say an I2C slave with 7-bit address 0x6F has a 64-byte SRAM at the register address range 32-95. We want to write a 16-byte character string to this SRAM, starting at register address 32.

The sequence would be as follows:

1. Software copies the 16-byte character string to the WBI2C proxy memory, starting at memory offset 32.
2. Software writes to the WBI2C CMD register specifying:
    - slave address: 0x6F
    - slave register address: 32
    - number of bytes: 16
    - the flag indicating that this is a write transaction.

3. WBI2C sends the 16 bytes to the I2C slave using a multi-byte transaction.
4. Software waits for transaction completion by polling the CMD register Busy bit, or by receiving the I2C IRQ.

To read back this 16-byte character string:

1. Software starts the transaction by writing to the WBI2C CMD register, specifying:
    - slave address: 0x6F
    - slave register address: 32
    - number of bytes: 16
    - the flag indicating that this is a read transaction.

2. WBI2C fetches the requested bytes from the I2C slave using a multi-byte transaction.
3. Software waits for transaction completion by polling the CMD Busy bit, or by receiving the I2C IRQ.
4. Software reads the 16-byte character string from the WBI2C proxy memory, starting at memory offset 32.

The I2C Test Application discussed [below](#the-i2c-test-application) implements this example.

### Where are the Output Enables?

The WBI2C core top-level has *SCL* and *SDA* input and output ports, but no *Output Enable* ports:

```
module wbi2cmaster #(
...
) (
    input wire i_clk,
    i_reset,
    // Wishbone
    ...
    // I2C clock and data wires
    input wire i_i2c_scl,
    input wire i_i2c_sda,
    output wire o_i2c_scl,
    output wire o_i2c_sda,

    // And our output interrupt
    output wire o_int,
    // And some debug wires
    output wire [31:0] o_dbg
);
```

How do you map a pair of unidirectional ports to a bidirectional inout port at the FPGA's top-level without an output enable signal to control the direction? The answer lies in the clever nature of I2C's physical layer. The SCL and SDA lines have external pull-ups:

![I2C bus with pull-up resistors](../assets/i2c_bus_w_pullups.png)

*I2C bus with pull-up resistors (Source: projectfpga.com)*

When neither master nor slave is driving, the lines are being pulled high. An I2C master or slave device just needs to drive the lines *low*. This means that the WBI2C core's SCL (SDA) output port can be used as the output enable of the top-level's bidirectional SCL (SDA) port:

| WBI2C SCL (SDA) output | Top-level SCL (SDA) | WBI2C SCL (SDA) input |
|---------------|---------------|-----------------|
| **1**      | output disabled, allowing<br>SCL (SDA) line to get pulled high | 1 or 0, taken from top-level SCL (SDA) pin |
| **0**      | output enabled, output value 0 | 0 |

Here is how I have it set up in BoxLambda's top-level:

```
module boxlambda_top (
    ...
    inout  wire i2c_scl,
    inout  wire i2c_sda,
    output wire i2c_scl_pup,  //SCL pull-up pin
    output wire i2c_sda_pup,  //SDA pull-up pin
    ...
)

  wire i2c_scl_i;     // SCL input to I2C core
  wire i2c_scl_o;     // SCL output from I2C core
  wire i2c_sda_i;     // SDA-line input to I2C core
  wire i2c_sda_o;     // SDA-line output from I2C core

  assign i2c_scl = i2c_scl_o ? 1'bZ : 1'b0;
  assign i2c_sda = i2c_sda_o ? 1'bZ : 1'b0;
  assign i2c_scl_i = i2c_scl_o ? i2c_scl : 1'b0;
  assign i2c_sda_i = i2c_sda_o ? i2c_sda : 1'b0;
  assign i2c_scl_pup = 1'b1;  //SCL pull-up pin.
  assign i2c_sda_pup = 1'b1;  //SDA pull-up pin.
```

### Pull-Up Pins

You might expect that the SCL and SDA pull-up resistors connect directly to the 3V3 power rail. On the Arty A7, that's not the case. The SCL and SDA pull-up resistors connect to an **SCL Pull-Up Pin** (SCL PUP) and **SDA Pull-Up Pin** (SDA PUP) respectively. For these pull-ups to work, the pull-up pins have to be statically driven high. That's what the last two lines in the above code snippet are for.

![SCL and SDA Pull-Up Pins on Arty A7 Schematic](../assets/scl_sda_pup_schematic.png)

*SCL and SDA Pull-Up Pins (PUP) on Arty A7 Schematic Diagram.*

I'm sure Digilent has good reasons for adding these pull-up pins, but I don't know what they are.

I2C Software API
================

[boxlambda/sw/components/i2c/i2c.h](https://github.com/epsilon537/boxlambda/tree/master/sw/components/i2c/i2c.h)

BoxLambda's I2C component is based on the [TinyWireM](https://github.com/adafruit/TinyWireM) API, which in turn is derived from Arduino's [Wire](https://www.arduino.cc/reference/en/language/functions/communication/wire/) API. This API makes it easy to port I2C-dependent components, such as the MCP79412RTC library, discussed [below](#rtcc-software-api).

A write operation to an I2C slave register typically looks like this:

```
  //Write to slave
  i2c.beginTransmission(I2C_SLAVE_ADDR);
  i2c.write(I2C_SLAVE_REG_ADDR);
  i2c.write(I2C_SLAVE_REG_VAL);
  i2c.endTransmission();
```

A read operation goes like this:

```
    //Read from slave

    i2c.beginTransmission(I2C_SLAVE_ADDR);
    i2c.write(I2C_SLAVE_REG_ADDR);
    i2c.endTransmission();

    i2c.requestFrom(I2C_SLAVE_ADDR, 1 /*Request 1 byte*/);
    uint8_t i2cSlaveRegVal = i2c.read();
```

### Slave Register Protocol not assumed

The I2C API doesn't assume that the first byte of an I2C data frame is a slave register address. For the I2C  API, the first byte of a data frame is just that. No special role is assigned to it. This is a good thing because it works. Even though the WBI2C clearly *is* built around the slave register address convention, it can work with an I2C slave that doesn't follow this convention.

Imagine an I2C slave that accepts a stream of data bytes, without any particular format, and you want to send the following four bytes to it: 255, 254, 253, 252. Using the I2C API, you would write:

```
  uint8_t bytes[4] = {255, 254, 253, 252};

  i2c.beginTransmission(I2C_SLAVE_ADDR);
  for (int ii=0; ii<4; ii++) i2c.write(bytes[ii]);
  uint8_t res = i2c.endTransmission();
```

The I2C driver code will configure WBI2C as follows:

1. It writes the following values to the WBI2C Proxy memory:

    | WBI2C Proxy Memory Address | Value |
    |----------------------------|-------|
    | 255 | 254 |
    | 0   | 253 |
    | 1   | 252 |

2. It writes to the WBI2C CMD register, specifying a write transaction, setting the slave register address to 255 and the number of bytes to 3.

This tricks WBI2C into 'thinking' it's writing to slave register addresses 255, 0, and 1. The resulting I2C data frame consists of the byte sequence 255, 254, 253, 252, as intended.

This works because:

1. WBI2C wraps around to proxy memory address 0 if a multi-byte transaction continues past address 255.
2. WBI2C is configured with a proxy memory size of 256 bytes. The address space covers all byte values.

The only limitation is that a bulk transaction can't be more than 257 bytes long, corresponding to one (fake) slave register address byte plus 256 proxy memory bytes.

The I2C Test Application
========================
The test code running on the RISCV processor: [boxlambda/sw/projects/rtcc_test/i2c_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/i2c_test/i2c_test.cpp)

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

The Real-Time Clock and Calendar (RTCC) Module
----------------------------------------------

BoxLambda uses I2C to interface with a [Real-Time Clock and Calendar PMOD](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/) from Digilent. This PMOD is powered by Microchip's MCP79410 and includes a coin cell battery backup so time and date can be maintained even while BoxLambda is switched off.

The MCP79410 datasheet: [http://ww1.microchip.com/downloads/en/DeviceDoc/22266d.pdf](http://ww1.microchip.com/downloads/en/DeviceDoc/22266d.pdf)

There aren't enough PMOD ports on the Arty A7 to accommodate all BoxLambda peripherals. Instead of using a PMOD port, I hooked up the RTCC PMOD to the I2C pins of the ChipKit Header:

| RTCC PMOD Pin | Arty A7 Board Pin |
|----------|-------------|
| 1 (*SCL*) | ChipKit Header SCL |
| 2 (*SDA*) | ChipKit Header SDA |
| 3 (*GND*) | ChipKit Header GND |
| 4 (*VCC*) | ChipKit Header IOREF |

![RTCC PMOD Setup.](../assets/rtcc_pmod.jpg)

*RTCC PMOD Setup.*

Clock Drift
===========

The MCP79410 uses a low-cost 32.768 kHz crystal. Typically, these crystals are not very accurate. Over time the clock will drift relative to the wall clock. However, Digilent's RTCC PMOD uses a *CMR200T_E* crystal with a reasonably good frequency tolerance of ±10ppm .

![CMT200T E](../assets/CMR200T_E.png)

*CMR200T E crystal.*

[http://cfd.citizen.co.jp/cms/cfd/pdf/english/CMR200T_E.pdf](http://cfd.citizen.co.jp/cms/cfd/pdf/english/CMR200T_E.pdf)

A 10ppm frequency error would result in 5 minutes of clock drift per year:

$$
\textrm{Clock Drift}=\textrm{365 days/yr}\times\textrm{24 h/day}\times\textrm{60 min./h}\times10/1000000\approx\textrm{5 min./yr}
$$

For BoxLambda's purposes, that's good enough. Additionally, the MCP79410 provides a digital timing compensation feature, so accuracy can be further improved if needed.

RTCC Software API
=================
[boxlambda/sub/MCP79412RTC/src/MCP79412RTC.h](https://github.com/epsilon537/MCP79412RTC/blob/boxlambda/src/MCP79412RTC.h)

The Real-Time Clock and Calendar (RTCC) software component is based on the [MCP79412RTC](https://github.com/epsilon537/MCP79412RTC) library and has the I2C software component as a dependency.

Example usage:

```
  /*Time elements structure*/
  tmElements_t tmElements;

  RTC.begin();

  if (RTC.isRunning()) {
    printf("RTC oscillator is already running.\n");
  }
  else {
    printf("RTC oscillator is not running yet. Enabling...\n");

    RTC.vbaten(true);

    /*Some bogus initial values.*/
    tmElements.Second = 1;
    tmElements.Minute = 2;
    tmElements.Hour = 3;
    tmElements.Wday = 4;   // day of week, sunday is day 1
    tmElements.Day = 5;
    tmElements.Month = 6;
    tmElements.Year = 7;   // offset from 1970;

    RTC.write(tmElements);
  }

  uint8_t prevSecond = 0;

  for (;;) {
    RTC.read(tmElements);

    /*print a line when the seconds change.*/
    if (tmElements.Second != prevSecond) {
      prevSecond = tmElements.Second;

      printf("sec: 0x%x, min: 0x%x, hour: 0x%x.\n", (unsigned)tmElements.Second, (unsigned)tmElements.Minute, (unsigned)tmElements.Hour);
    }
  }
```

The RTCC Test Application
=========================

- [boxlambda/sw/projects/rtcc_test/rtcc_test.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_test.cpp)
- [boxlambda/sw/projects/rtcc_test/rtcc_cli.cpp](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/rtcc_test/rtcc_cli.cpp)

The Real-Time Clock and Calendar test program does the following:

1. Boot from Flash Memory.
2. Every second, retrieve the current time from the RTCC PMOD via I2C.
3. Drop into a CLI when button 0 is pressed.
4. From the CLI, the user can set the time, get the time, set the date, and get the date.

This test program does *not* run on Verilator.

Other Changes
-------------

I added a reset test to check the software interaction with BoxLambda's reset controller. The test does the following:

1. Retrieve the reset reason after reset (POR, External Reset, SW triggered reset...).
2. Trigger a system reset via software.

Try It Yourself
---------------

Setup
=====
1. Install the [Software Prerequisites](https://boxlambda.readthedocs.io/en/latest/prerequisites/).
2. Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
1. Switch to the **i2c_rtcc** tag:
```
git checkout i2c_rtcc
```
1. Set up the repository. This initializes the git submodules used and creates the default build trees:
```
./boxlambda_setup.sh
```

The I2C Test on Verilator
=========================

1. Build the **i2c_test** project:

    ```
    cd build/sim-a7-100/gw/projects/i2c_test
    make i2c_test_sim_sw
    ```

2. Execute the generated Verilator model. You should see the following output:

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

The I2C Test on FPGA
====================

1. Hook up the RTCC PMOD as described [here](#the-real-time-clock-and-calendar-rtcc-module).

2. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.

3. Build the project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):

    ```
    cd build/arty-a7-100/gw/projects/i2c_test
    make i2c_test_bit_sw
    ```

4. Download the generated bitstream file to the Arty A7:

    ```
    make i2c_test_load
    ```

5. You should see the following output in the terminal:

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

6. Press button 0 to get dropped into the CLI. Type *help* to see a list of available commands:

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

7. Using the CLI, issue an I2C transaction. For example, read I2C slave register 0:

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

The RTCC Test on FPGA
=====================

1. Hook up the RTCC PMOD as described [here](#the-real-time-clock-and-calendar-rtcc-module).

2. Connect a terminal program to Arty's USB serial port. **Settings: 115200 8N1**.

3. Build the *rtcc_test* software project in an Arty A7 build tree:

    ```
    cd build/arty-a7-100/sw/projects/rtcc_test
    make rtcc_test
    ```

4. Flash the *rtcc_test* program onto the target:

    ```
    make rtcc_test_flash_sw
    ```

5. Build the rtcc_test gateware project in an Arty A7 build tree (*arty-a7-35* or *arty-a7-100*):

    ```
    cd build/arty-a7-100/gw/projects/rtcc_test
    make rtcc_test_bit_sw
    ```

6. Flash the gateware build onto the target:

    ```
    make rtcc_test_flash_gw
    ```

7. When flashing has been completed, the target will be reset. In the terminal, you should now see the following output:

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

8. Power down the Arty A7, wait a while, then power it back up.

9. You should see time running with the powered-down time gap accounted for. After power-up time should **not** be reset back to 0x1:0x2:0x3 :

    ```
    RTC oscillator is already running.
    Polling time in a loop. Push btn[0] to end loop and start CLI.
    sec: 0x1f, min: 0x22, hour: 0x3.
    sec: 0x20, min: 0x22, hour: 0x3.
    sec: 0x21, min: 0x22, hour: 0x3.
    sec: 0x22, min: 0x22, hour: 0x3.
    sec: 0x23, min: 0x22, hour: 0x3.
    ```

10. Press button 0 to drop into the CLI, then type 'help' to see a list of available commands:

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

11. Play with the settime/gettime/setdate/getdate commands. For example:

    ```
    > setdate 7 8 2024
    Done.
    > getdate
    The current date is (dd/mm/yyyy): 07/08/2024.
    ```

Reset Test on Verilator
=======================

1. Build the test project:

    ```
    cd build/sim-a7-100/gw/projects/reset_test
    make reset_test_sim_sw
    ```

2. Execute the generated verilator model:

    ```
    ./Vmodel
    ```

3. You should see something like this:

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

Reset Test Image on FPGA
========================

1. Build the test project:

    ```
    cd build/arty-a7-[35|100]/gw/projects/reset_test
    make reset_test_bit_sw
    ```

2. Connect a terminal program such as Putty or Teraterm to Arty's USB serial port. **Settings: 115200 8N1**.

3. Run the project:

    ```
    make reset_test_load
    ```

4. Verify that software reports Power-On Reset as the reset reason:

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

5. Press button 0, which should cause software to issue a reset request for the Debug and Non-Debug Module reset domains. The system should restart and software should report a software-triggered Non-Debug Module and Debug Module reset reason:

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

6. Press the external reset button. The system should restart and software should report an *External Reset* reason:

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

Conclusion
----------
With the addition of the WBI2C core, the **BoxLambda Little** SoC configuration for the Arty-A7-35T is gateware complete. I'm sure there will be changes once I start building a run-time software platform on top of it, but in terms of gateware architecture and features, this is what I had in mind when I started the project.

For the **BoxLambda Big** configuration, I still have a major feature to tackle: *Dynamic Function Exchange*, a.k.a. partial FPGA reconfiguration. With some luck, that'll be the topic of the next post.

References
----------
I2C Tutorials:

- [https://www.robot-electronics.co.uk/i2c-tutorial](https://www.robot-electronics.co.uk/i2c-tutorial)

- [https://learn.sparkfun.com/tutorials/i2c/all](https://learn.sparkfun.com/tutorials/i2c/all)

- [https://ai.thestempedia.com/docs/evive/evive-technical-specifications/arduino-core-interface/i2c-communication/](https://ai.thestempedia.com/docs/evive/evive-technical-specifications/arduino-core-interface/i2c-communication/)

- [https://en.wikipedia.org/wiki/I%C2%B2C](https://en.wikipedia.org/wiki/I%C2%B2C)

[TinyWireM](https://github.com/adafruit/TinyWireM)

[MCP79410 Datasheet](http://ww1.microchip.com/downloads/en/DeviceDoc/22266d.pdf)

[Digilent Real-Time Clock and Calendar PMOD](https://digilent.com/shop/pmod-rtcc-real-time-clock-calendar/)

