# Building Software

The software corresponding to a gateware project is automatically compiled, converted to a memory file, and included in the gateware project as part of the build process (see gateware build rules **<project>_bit_sw** and **<project>_sim_sw**).

Software projects can also be built independently. From the build directory, type:

- To create a RAM Build: `make <sw_project_name>_ram`.
- To create a Flash Memory Build: `make <sw_project_name>_flsh`.

### Example:

```
$ cd build/sim-a7-100/sw/projects/hello_world/
$ make hello_world_ram
...
$ ls
CMakeFiles           CTestTestfile.cmake  hello_world_ram.bin  hello_world_ram.map  Makefile
cmake_install.cmake  hello_world_ram      hello_world_ram.hex  hello_world_ram.mem
```

To flash a flash memory software image onto the target, type `make <sw_project_name>_flsh_flash_sw`.

### Example:

```
$ cd build/arty-a7-100/sw/projects/hello_world/
$ make hello_world_flsh
$ make hello_world_flsh_flash_sw
```
