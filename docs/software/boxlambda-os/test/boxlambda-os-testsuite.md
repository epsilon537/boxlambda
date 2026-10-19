# The BoxLambda OS Test Suite

The BoxLambda OS test suite is based on the Mecrisp Forth test suite, augmented with features specifically added for BoxLambda (C-FFI, VERA driver,...).

## Building and Running the Test Suite

1. Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 1000000 8N1**.

2. Ensure all release binaries are flashed onto the target. If that's not the case yet, follow the steps in [this section](../../../installation/installation.md#installing-the-boxlambda-base-bitstream-bootloader-and-os).

3. Set switch 1 to *On*, switches 0, 2 and 3 to *Off*. This tells the BoxLambda OS to start executing the testsuite after boot.

4. Boot BoxLambda OS with RAM disk. From the worksace root directory, run the following command: 

```
target.py -load_app binaries/boxkernforth.bin -load_fs fs -run
```

After booting, the test suite will automatically start executing. You can follow along on the console. The test suite will take a few minutes to complete.

```
BoxLambda bootloader
--------------------
Version: v0.4.0-devel-rc0-118-gc1aaa73b7-dirty
Initializing SDRAM...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  m0, b01: |01111111111111111111111111111100| delays: 14+-14
  m0, b02: |00000000000000000000000000000000| delays: -
  m0, b03: |00000000000000000000000000000000| delays: -
  m0, b04: |00000000000000000000000000000000| delays: -
  m0, b05: |00000000000000000000000000000000| delays: -
  m0, b06: |00000000000000000000000000000000| delays: -
  m0, b07: |00000000000000000000000000000000| delays: -
  best: m0, b01 delays: 15+-14
  m1, b00: |00000000000000000000000000000000| delays: -
  m1, b01: |01111111111111111111111111111100| delays: 15+-14
  m1, b02: |00000000000000000000000000000000| delays: -
  m1, b03: |00000000000000000000000000000000| delays: -
  m1, b04: |00000000000000000000000000000000| delays: -
  m1, b05: |00000000000000000000000000000000| delays: -
  m1, b06: |00000000000000000000000000000000| delays: -
  m1, b07: |00000000000000000000000000000000| delays: -
  best: m1, b01 delays: 15+-14
Switching SDRAM to hardware control.
Done.
Application image magic number check OK.
Application image size: 471500 bytes
Copying SW image from Flash to DDR...
Done.
Booting application image...
Initializing Forth...

Mecrisp-Quintus 1.1.1 for RISC-V RV32IM by Matthias Koch.
BoxLambda port by Ruben Lysens/Epsilon537.
Forth core init complete.
Mounting file system...
CID: 534d5402:47323341:7d604971:3168018d
sd0: mounted.
Boot path sd0:forth found.
No FatFS detected on ram:.
Booting from volume sd0:.
Loading forth/early.fs...
Redirecting stdio to Forth...
Initializing Forth Filesystem FFI...
Loading fastboot forth image...
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
Redefine foo.
Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned:
88 77
test_c_fun called with args 22 and 11.
Redefine foo.
Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned:
88 77
test_c_fun called with args 22 and 11.
Redefine foo.
Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned:
88 77
test_c_fun called with args 22 and 11.
Redefine foo.
Calling test Word foo in Forth with args 11 22...
In foo...11 22
Foo returned 33.
Returning values 77 88 to Forth.
test_c_fun returned:
88 77
Executing .s:
Stack: [ 0 ]  TOS: 42  *>
Executing forth/init.fs...
cd /test/vera/
...
```

The test leaves you at the Forth prompt.

