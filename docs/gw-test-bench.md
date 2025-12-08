# The Test Bench

Because BoxLambda primarily integrates existing components that have already been verified by their respective owners, the focus is on system-level testing rather than component-level verification. In the few areas where BoxLambda introduces significant new logic, component and/or module-level verification is performed. Component-level verification and system-level testing use different test benches.

## Running Regression Tests

CMake comes with a regression test framework called **Ctest**. To see a list of available test cases, you need to first build everything and then run the `ctest -N` command to list the test cases:

```bash
cd <boxlambda root dir>/build/sim-a7-100
make all
ctest -N
```

You should see something like this:

```
Test project /home/epsilon/work/boxlambda/build/sim2
  Test #1: hello_world_test
  Test #2: hello_dbg_test
  Test #3: picolibc_test_test
  Test #4: ddr_test_test

Total Tests: 4
```

To run a specific test, use the following command from the build directory:

```bash
ctest -I <test number>
```

To run all tests, simply run the `ctest` command without any parameters.

Currently, ctest in the `sim-a7-100` buil tree only runs gateware test cases, while ctest in the `arty-a7-100` build tree runs software testcases against a connected FPGA target.

