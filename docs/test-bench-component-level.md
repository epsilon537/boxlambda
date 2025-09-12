---
hide:
  - toc
---
# The Component-Level Test Bench

[CoCoTB](https://www.cocotb.org/) is used for component-level verification, with [Icarus](https://steveicarus.github.io/iverilog/) acting as the behind-the-scenes simulator for CoCoTB.

An example of a CoCoTB test bench is the Ibex *Single Instruction Prefetcher* test bench. This is the test script:

[https://github.com/epsilon537/boxlambda/blob/master/gw/components/ibex/test/ibex_single_prefetch_test.py](https://github.com/epsilon537/boxlambda/blob/master/gw/components/ibex/test/ibex_single_prefetch_test.py)

The test bench is added as a CMake test to the build system, like this:

```
add_test(NAME ibex_no_prefetch_test
    COMMAND ${PROJECT_SOURCE_DIR}/scripts/cocotb_test.sh ${CMAKE_CURRENT_LIST_DIR}/test/ibex_single_prefetch_test.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)
```

Note that a `cocotb_test.sh` wrapper script is used to execute the test. This script sets up the environment and ensures that a `.fst` waveform is generated when the test is run.

Run the test as follows:

```
~/work/boxlambda/build/sim-a7-100/gw/components/ibex$ ctest -V
...
1: Test command: /home/epsilon/work/boxlambda/scripts/cocotb_test.sh "/home/epsilon/work/boxlambda/gw/components/ibex/test/ibex_single_prefetch_test.py"
...
```

The waveform is available in the `ibex_single_prefetch_test_sim_build` subdirectory.

