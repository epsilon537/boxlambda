# Building the Software

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

