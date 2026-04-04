# The OS Memory Layout and Boot Sequence

## Memory Layout


[![BoxLambda OS
Memory Layout.](assets/os_mem_layout.png)](assets/os_mem_layout.png)

*BoxLambda OS Memory Layout.*

### Memory Footprint

TBD

## Boot Sequence

This section describes the OS Boot Sequence. It starts where the [Bootloader Boot Sequence section](sw_bootloader.md#boot-sequence) leaves off.

After the Bootloader has transferred control to the OS image it has loaded into EMEM, the OS start-up code goes through the following sequence:

1. `crt0.c` sets up a basic C environment as described [here](c_comp_picolibc.md#software-startup-sequence), unpacking and initializing the different sections into IMEM and EMEM as shown in the memory layout diagram above.
2. BoxLambda OS's `main()` initializes Forth by calling `forth_core_init()`.
3. `main()` loads [init.fs](https://github.com/epsilon537/boxlambda/blob/master/sw/components/forth_core/init.fs) into the Forth environment using `forth_load_buf()`.
4. `main()` invokes the `welcome` Word followed by a `Ready.` statement.
5. `main()` transfers control to the Forth REPL by calling `forth_repl()`.




