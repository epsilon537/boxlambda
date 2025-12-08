# Building the Software for BoxLambda

To build a software project, from the build directory, enter the following command:

`make <sw_project_name>`.

Example:

```
$ cd build/sim-a7-100/sw/projects/hello_world
$ make hello_world
...
$ ls
CMakeFiles           CTestTestfile.cmake  hello_world.bin  hello_world.map  Makefile
cmake_install.cmake  hello_world          hello_world.hex  hello_world.mem
```

To flash a software image onto the target, run `make <sw_project_name>_flash_sw`.

Example:

```
$ cd build/arty-a7-100/sw/projects/hello_world
$ make hello_world
$ make hello_world_flash_sw
```

To build all software projects (excluding the gateware builds), run `make sw` from the root of the build directory.

For gateware test builds, the software associated with a gateware project is automatically compiled, converted to a memory file, and included in the gateware project as part of the build process.

## The Cross-Compiler

The RISCV cross-compiler is a custom-built **riscv32-boxlambda-elf** toolchain. The compiler build is created using the excellent [Crosstool-ng](https://crosstool-ng.github.io/) project. *Crosstool-ng* uses a menuconfig similar to the Linux kernel menuconfig. You just focus on the specifics of the toolchain you want to build (RISC-V, 32-bit, Static Toolchain...). The tool selects good defaults for all the rest.

I selected:

- Target Architecture: *riscv*
- Architecture level: *rv32im_zicsr_za_zb_zbs*
- ABI: *ilp32*
- *Build Static Toolchain*
- Tuple's vendor string: *boxlambda*
- Target OS: *bare-metal*
- Additional support languages: *C++*

The resulting crosstool-ng config file can be found [here](https://github.com/epsilon537/boxlambda/blob/master/scripts/crosstool-ng.config).

The toolchain tarball is checked into the BoxLambda repo under [assets/](https://github.com/epsilon537/boxlambda/tree/master/assets). The [boxlambda_setup.sh](https://github.com/epsilon537/boxlambda/blob/master/boxlambda_setup.sh) script unpacks the toolchain tarball in the `boxlambda/tools/` directory, so the user no longer needs to provide the toolchain as a prerequisite.

### RISC-V GCC Compile Flags

The following compile flags are used:

- `march=rv32im_zba_zbb_zbs_zicsr`:
    - `rv32`: 32-bit RISC-V base architecture.
    - `i`: Base integer instruction set (mandatory).
    - `m`: Integer multiplication and division extension.
    - `zba`: Bit-Manipulation instructions for address generation.
    - `zbb`: Bit-Manipulation Base sub-extension.
    - `zbs`: Single-bit instructions.
    - `zicsr`: Control and Status Register (csr) instructions.

- `-mabi=ilp32`:
    - `i`: Integer-based ABI (does not use floating-point registers for function arguments/returns).
    - `l`: Long and int types are 32-bit.
    - `p`: Pointers are 32-bit wide.
    - `32`: Indicates a 32-bit ABI (used for RV32 architectures).

- `Wl,--no-warn-rwx-segments`: Avoid linker warning because executable segments are writable. This is intentional on BoxLambda.

### Toolchain.cmake

RISC-V cross-compilation for C and C++ is set up by passing in a *Toolchain File* to CMake. The toolchain file specifies the names of the compiler executables and the compile flags. The file is located in [scripts/toolchain.cmake](https://github.com/epsilon537/boxlambda/blob/master/scripts/toolchain.cmake).

