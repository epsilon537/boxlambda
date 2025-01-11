## The BoxLambda Base Project Build

- **BoxLambda Base Project in the BoxLambda Directory Tree**:  
  [boxlambda/gw/projects/boxlambda-base](https://github.com/epsilon537/boxlambda/tree/master/gw/projects/boxlambda_base)

This project builds the 'official' BoxLambda Base Configuration, as described in the [Architecture section](architecture.md#the-base-configuration).

This is an FPGA-only project build. Most test project builds are variants of this project, tailored for testing a specific feature, and include a Verilator version as well.

This build expects to find the software image to boot in flash memory.

### Building and Running BoxLambda Base

Connect a terminal program to the Arty's USB serial port. **Settings: 115200 8N1**.

Build the flash image for one of the non-DFX software projects in the Arty A7 build tree. Using the *ddr_test* flash image as an example:

```
cd build/arty-a7-100/sw/projects/ddr_test
make ddr_test_flsh
```

Flash the software image onto the target:

```
make ddr_test_flsh_flash_sw
```

Build the `boxlambda_base` bitstream in the Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/boxlambda_base
make boxlambda_base_bit
```

Flash the bitstream onto the target:

```
make boxlambda_base_flash_gw
```

Once flashing is complete, the target will be reset. In the terminal, you should now see the software image booting up.

