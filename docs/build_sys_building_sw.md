# Building Software for BoxLambda

The software associated with a gateware project is automatically compiled, converted to a memory file, and included in the gateware project as part of the build process (see gateware build rules **<project>_bit_sw** and **<project>_sim_sw**).

Software projects can also be built independently. From the build directory, enter the following command:

- To create a RAM Build: `make <sw_project_name>_ram`.
- To create a Flash Memory Build: `make <sw_project_name>_flsh`.

Example:

```
$ cd build/sim-a7-100/sw/projects/hello_world
$ make hello_world_ram
...
$ ls
CMakeFiles           CTestTestfile.cmake  hello_world_ram.bin  hello_world_ram.map  Makefile
cmake_install.cmake  hello_world_ram      hello_world_ram.hex  hello_world_ram.mem
```

To flash a software image onto the target, run `make <sw_project_name>_flsh_flash_sw`.

Example:

```
$ cd build/arty-a7-100/sw/projects/hello_world
$ make hello_world_flsh
$ make hello_world_flsh_flash_sw
```

## RISC-V GCC Compiler Flags

BoxLambda software is built using the *riscv64-unknown-elf-gcc* compiler. The following flags are used:

- `-march=rv32im_zicsr`:
    - rv32: 32-bit RISC-V base architecture.
    - i: Base integer instruction set (mandatory).
    - m: Integer multiplication and division extension.
    - zicsr:
        - This is a Z-prefixed extension, meaning it's a modular standard extension.
        - zicsr: Stands for "Control and Status Register (csr) instructions."
        - It includes instructions like *csrrw*, *csrrs*, and *csrrc* for accessing control registers, which are essential for system-level programming.

- `-mabi=ilp32`:
    - i: Integer-based ABI (does not use floating-point registers for function arguments/returns).
    - l: Long and int types are 32-bit.
    - p: Pointers are 32-bit wide.
    - 32: Indicates a 32-bit ABI (used for RV32 architectures).

The compiler flags are defined in [scripts/toolchain.cmake](https://github.com/epsilon537/boxlambda/blob/master/scripts/toolchain.cmake).
