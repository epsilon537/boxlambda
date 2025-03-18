---
hide:
  - toc
---

## Prerequisites

### Host OS

Linux. The package installation commands below are Ubuntu-based. I also have a BoxLambda setup on Fedora and Arch.

### Vivado

**Vivado ML** Edition V2023.1, Linux version:

[https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2023-1.html](https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2023-1.html)

Make sure you also install your Arty A7 board files. Digilent has excellent instructions for installing Vivado and Digilent board files:

[https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis](https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis)

### Riscv Toolchain

```
sudo apt install gcc-riscv64-unknown-elf
```

### GNU Make

Version 4.2.1 or later.

```
sudo apt-get install make
```

### CMake

Version 3.24 or later.

On Ubuntu, follow these instructions: [https://apt.kitware.com/](https://apt.kitware.com/)

Make sure that the correct CMake version is first in your PATH. The Vivado */tools/Xilinx/Vivado/2023.1/settings64.sh* script adds an old version of CMake to your path. I override that by re-adding */usr/bin* to the front of the PATH after sourcing the Vivado script:

```
source /tools/Xilinx/Vivado/2023.1/settings64.sh
export PATH=/usr/bin:$PATH
```

### GDB Multiarch

For debug access to BoxLambda.

```
sudo apt install gdb-multiarch
```

### Additional Prerequisites for Building and Running the Verilator Test Cases

#### SDL2

SDL2 is needed to run Verilator simulation test cases involving the VERA graphics core.

[https://wiki.libsdl.org/SDL2/Installation](https://wiki.libsdl.org/SDL2/Installation)

Also, set the environment variable **SDL2_DIR** to point to the SDL2 directory containing files *SDL2Config.cmake* or *sdl2-config.cmake*. In my case (Ubuntu 22.04), I added the following line to my *~/.bashrc*:

```
export SDL2_DIR=/usr/lib/x86_64-linux-gnu/cmake/SDL2/
```

#### Chromaprint

Chromaprint is needed to run Verilator simulation test cases involving the YM2149 PSG sound core.

```
sudo apt-get install libchromaprint-tools
```

### Tools you probably don't need

#### Gforth

Gforth is required if you would like to tinker with the usb_hid_device firmware.

```
sudo apt-get install gforth
```
#### Ninja and Meson

Ninja and Meson are required if you want to rebuild the Picolibc library.

```
sudo apt-get install ninja-build
sudo apt-get install meson
```

#### Perl

Perl is required if you would like to tinker with the usb_hid_host firmware. Your system most likely already has Perl installed. If not:

```
sudo apt-get install perl.
```

### Hidden Tool Dependencies

The following tools are automatically installed (if not already installed) by sourcing the *activate_env.sh* script:

- [OSS CAD Suite (CoCoTB, iverilog, GTKWave, openFPGALoader, OpenOCD, Python)](https://github.com/YosysHQ/oss-cad-suite-build)
- [Bender](https://github.com/pulp-platform/bender)
- Python packages NumPy, SciPy, and Matplotlib.
- [BoxLambda fork of Litex](https://github.com/epsilon537/litex)

