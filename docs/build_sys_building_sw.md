Building Software
-----------------
The software corresponding with a gateware project automatically gets compiled, converted to a memory file, and included in the gateware project as part of the build process (see gateware build rules **<project\>_bit_sw** and **<project\>_sim_sw**). 

Software projects can also be built independently. From the build directory just type: `make <sw project name>`. For example:

```
$ cd sim-a7-100/sw/projects/hello_world/
$ make hello_world
...
$ ls
CMakeFiles           Makefile             hello_world      hello_world.map
CTestTestfile.cmake  cmake_install.cmake  hello_world.hex  hello_world.mem
```

Some software images are built to boot from flash memory instead of internal memory. To flash such a software image onto the target, type `make <sw_project_name>_flash_sw`.
