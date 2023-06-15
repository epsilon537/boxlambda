## Hello World

### Hello World on the Arty A7-35T

Project directory **boxlambda/gw/projects/hello_world/** contains a test SoC build consisting of an Ibex_WB core, 64KB internal memory, a wbuart32 core, a timer, and a GPIO module.

To build the *Hello World!* example, go through the following steps:

Build the project:
```
cd build/arty-a7-[35|100]/gw/projects/hello_world
make hello_world_bit_sw
```
Download the bitstream to the target:
```
make hello_world_load
```

### Hello World Verilator Build

To try out the Verilator Test Bench for *Hello World*:

Build the testbench:
```
cd build/sim-a7-100/gw/projects/hello_world
make hello_world_sim_sw
```
Execute the testbench, with (```./Vmodel -i```) or without (```./Vmodel -t```) tracing:
```
./Vmodel -i/-t
```
View the generated traces: 
```
gtkwave simx.fst
```

