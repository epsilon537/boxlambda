# Hello World

This is the canonical *Hello World* test. It boots up the C environment
on the SoC, prints out `Hello World!`, and then just spins in a loop.

## On the Arty A7

To build and run the *Hello World* example, go through the following steps:

Build the project:
```
cd build/arty-a7-100/gw/projects/hello_world
make hello_world_bit
```
Download the bitstream to the target:
```
make hello_world_load
```

## On Verilator

To try out the Verilator Test Bench for *Hello World*:

Build the testbench:
```
cd build/sim-a7-100/gw/projects/hello_world
make hello_world_sim
```
Execute the testbench, with (```./Vmodel -i```) or without (```./Vmodel -t```) tracing:
```
./Vmodel -i/-t
```
View the generated traces:
```
gtkwave simx.fst
```

