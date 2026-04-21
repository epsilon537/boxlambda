# The Forth Test Suite

**The software project directory**: [sw/projects/boxlambda_os](../../../../../sw/projects/boxlambda_os)

This build runs the Forth regression test suite. The testsuite is based on the Mecrisp
Forth testsuite, augmented with Words specifically added for BoxLambda (C-FFI, `c,`, `halign`, `compiletoimem`,...).

## Building and Running the Forth Test Suite on FPGA

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 1000000 8N1**.

Make sure the `boxlambda_base` release bitstream is flashed onto the target. If that's not the case yet, follow the steps in [this section](../../../../installation/installation.md#installing-the-boxlambda-base-bitstream-bootloader-and-os).

Build the Forth Test Suite and load the test binary and file system onto the target:

```
cd build/arty-a7-100/sw/projects/boxlambda_os
make boxkerntestfs_load
```

You should see the following output on the serial port terminal:

```
BoxLambda bootloader
--------------------
Version: v0.4.0
Initializing SDRAM...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  ...
  best: m1, b01 delays: 14+-14
Switching SDRAM to hardware control.
Done.
Spinning while switches are all 0.
Initializing Forth...

Mecrisp-Quintus 1.1.1 for RISC-V RV32IM by Matthias Koch.
BoxLambda port by Ruben Lysens/Epsilon537.
Forth core init complete.
Mounting file system...
CID: 534d5402:47323341:7d604971:3168018d
sd0: mounted.
No boot path found on sd0:.
ram: mounted.
Boot path ram:forth found.
Booting from volume ram:.
Loading forth/early.fs...
Redirecting stdio to Forth...
Initializing Forth Filesystem FFI...
Parsing forth/boxkern-includes.fss...
Loading forth/utils.fs...
Redefine .".
Loading forth/except.fs...
Loading forth/lambda.fs...
Loading forth/struct.fs...
...
Loading forth/fs.fs...
Loading forth/fs-redirect.fs...
Loading forth/shell.fs...
Redefine _cp-file-to-file.
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
test_c_fun returned:
88 77
test_c_fun called with args 22 and 11.
...
Returning values 77 88 to Forth.
test_c_fun returned:
88 77
Executing .s:
Stack: [ 0 ]  TOS: 42  *>
Redefine variable.
Redefine 2variable.
Stack: [ 0 ]  TOS: 2A  *>
Redefine ?assert.

TESTING BASIC ASSUMPTIONS
...
TESTING ESCAPED STRINGS
TESTING CONDITIONAL COMPILATION
TESTING COMPLETE
Stack: [ 68 ] 2A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  TOS: 0  *>

     _
    ^-)
     (.._          .._
      \`\\        (\`\\        (
       |>         ) |>        |)
______/|________ (7 |` ______\|/_______a:f

Ready.


ram:/>
```

The test leaves you at the Forth prompt.

