# The Forth Core Test Build

**The software project directory**: [sw/projects/forth_core_test](https://github.com/epsilon537/boxlambda/tree/master/sw/projects/forth_core_test)

This build runs the Forth regression test suite. The testsuite is based on the Mecrisp
Forth testsuite, augmented with Words specifically added for BoxLambda (C-FFI, c, halign, compiletoimem,...).

The program expects user input at some point. To automate this part, the program can be executed under control of a `pexpect` based Python script that will provide the required input at the right time. The Python script also checks the expected output against a [reference output file](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/forth_core_test/test/forth_core_test.output.txt) and provide the overall pass/fail status to the ctest bench.

## Building and Running Forh Core Test on FPGA

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 1000000 8N1**.

Make sure the `boxlambda_base` release bitstream is flashed onto the target. If that's not the case yet, follow the steps in [this section](installation.md#installing-the-boxlambda-base-bitstream-bootloader-and-os).

Build the Forth Core Test software project:

```
cd build/arty-a7-100/sw/projects/forth_core_test
make forth_core_test
```

### Running Forth Core Test Manually

At this point you could just flash the software onto the target using the command `make forth_core_flash_sw`. I avoid that, however, because I'm worried about wearing out the board's flash memory. Instead, I use OpenOCD+GDB to load the software onto the target:

Set the board swithes all to off. This causes the bootloader to spin after booting up, allowing OpenOCD/GDB to connect.

Open a new terminal window, activate the boxlambda environment and start OpenOCD:

```
cd <boxlambda root dir>
source activate_env.sh
openocd_arty_a7_100t.sh -r
```

Now go back to the terminal where you build `forth_core_test` and load the program using GDB:

```
gdb forth_core_test
(gdb) target remote localhost:3333
(gdb) load
(gdb) continue
```

You should see the following output on the serial port terminal:

```
BoxLambda bootloader
--------------------
Version: v0.3.0
Initializing SDRAM...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  m0, b01: |11111111111111111111111111111000| delays: 14+-14
  m0, b02: |00000000000000000000000000000000| delays: -
  m0, b03: |00000000000000000000000000000000| delays: -
  m0, b04: |00000000000000000000000000000000| delays: -
  m0, b05: |00000000000000000000000000000000| delays: -
  m0, b06: |00000000000000000000000000000000| delays: -
  m0, b07: |00000000000000000000000000000000| delays: -
  best: m0, b01 delays: 14+-14
  m1, b00: |00000000000000000000000000000000| delays: -
  m1, b01: |11111111111111111111111111111100| delays: 14+-14
  m1, b02: |00000000000000000000000000000000| delays: -
  m1, b03: |00000000000000000000000000000000| delays: -
  m1, b04: |00000000000000000000000000000000| delays: -
  m1, b05: |00000000000000000000000000000000| delays: -
  m1, b06: |00000000000000000000000000000000| delays: -
  m1, b07: |00000000000000000000000000000000| delays: -
  best: m1, b01 delays: 14+-14
Switching SDRAM to hardware control.
Done.
Done.
Application image magic number check OK.
Application image size: 77004 bytes
Copying SW image from Flash to DDR...
Done.
Spinning while switches are all 0.

Mecrisp-Quintus 1.1.1 for RISC-V RV32IM by Matthias Koch.
BoxLambda port by Ruben Lysens/Epsilon537.
Forth init complete.
Compiling Forth included_tools...
Redefine forget. Redefine u.4. Redefine u.2. Forth-C FFI testcases...
Checking forth_find_word(quit) against ' quit. Diff should be 0:
0
Forth computing 3 4 +:
Result: 7
Forth evaluating string: 3 4 + . cr
7
Forth evaluating string: 42 emit cr
*
Registering test_c_fun with Forth...
Forth evaluating string: 11 22 test_c_fun . . cr with various RS alignments...
test_c_fun called with args 22 and 11.
Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned 88 77
test_c_fun called with args 22 and 11.
Redefine foo. Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned 88 77
test_c_fun called with args 22 and 11.
Redefine foo. Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned 88 77
test_c_fun called with args 22 and 11.
Redefine foo. Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned 88 77
test_c_fun called with args 22 and 11.
Redefine foo. Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned 88 77
Executing .s:
Stack: [ 0 ]  TOS: 42  *>
Executing Forth testsuite...
Redefine nexttoken. Redefine [else]. Redefine [then]. Redefine [if]. Redefine [ifdef]. Redefine [ifndef]. Redefine variable. Re
define 2variable. Redefine s>f.
TESTING CORE WORDS
TESTING BASIC ASSUMPTIONS
TESTING BOOLEANS: INVERT AND OR XOR
...
PLEASE TYPE UP TO 80 CHARACTERS:
```

Type some characters and press enter. The test continues:

```
RECEIVED: "    sfdgj"
TESTING DICTIONARY SEARCH RULES
...
TESTING COMPILETOIMEM/EMEM
TESTING COMPLETE
Stack: [ 0 ]  TOS: 42  *>


     _
    ^-)
     (.._          .._
      \`\\        (\`\\        (
       |>         ) |>        |)
______/|________ (7 |` ______\|/_______a:f

```

The test leaves you at the Forth prompt.

### Running Forth Core Test Automatically

The Forth Core Test can also be run automatically, under control of a Python script. To kick off the automatic test:

If you have it open, close the serial port terminal. The Python script will take control of the serial port.

Navigate to the `sw/projects/forth_core_test/test` directory in the **source tree** (not the build tree) and run the python script:

```
cd sw/projects/forth_core_test/test
python3 forth_core_test.py
```

You should see the following output:

```
Board Reset and OpenOCD start...
Resetting target...
...
Checking output against reference...
Test passed.
Terminating...
```

