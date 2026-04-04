# Prerequisites

## Host OS

Linux. The package installation commands below are Ubuntu-based. I also have a BoxLambda setup on Fedora and Arch.

## Vivado

**Vivado ML** Edition V2025.1, Linux version:

[https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2025-1.html](https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2025-1.html)

Make sure you also install your Arty A7 board files. Digilent has excellent instructions for installing Vivado and Digilent board files:

[https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis](https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis)

## Git

```
sudo apt-get install git
```

## GNU Make

Version 4.2.1 or later.

```
sudo apt-get install make
```

## CMake

Version 4.0.0 or later.

On Ubuntu, follow these instructions: [https://apt.kitware.com/](https://apt.kitware.com/)

Make sure that the correct CMake version is first in your PATH. The Vivado `/tools/Xilinx/Vivado/2023.1/settings64.sh` script adds an old version of CMake to your path. I override that by re-adding `/usr/bin` to the front of the PATH after sourcing the Vivado script:

```
source /tools/Xilinx/Vivado/2023.1/settings64.sh
export PATH=/usr/bin:$PATH
```

### Ninja and Meson

Ninja and Meson are required to build the Picolibc library.

```
sudo apt-get install ninja-build
sudo apt-get install meson
```

## GDB (Multiarch)

For debug access to BoxLambda.

```
sudo apt install gdb-multiarch
```

On Arch Linux, the regular `gdb` package includes multiarch support, so you should just install `gdb` instead of `gdb-multiarch`.

## Additional Prerequisites for Building and Running the Verilator Test Cases

### SDL2

If SDL2 is installed, the `vera_integrated_test` Verilator simulation test cases will use it to display the VERA graphics core VGA output. If SDL2 is not installed, the test will still run, using the same pass/fail criteria, but the output will not be rendered to a window.

[https://wiki.libsdl.org/SDL2/Installation](https://wiki.libsdl.org/SDL2/Installation)

### Chromaprint

Chromaprint is needed to run Verilator simulation test cases involving the YM2149 PSG sound core.

```
sudo apt-get install libchromaprint-tools
```

## Tools you probably don't need

### Gforth

Gforth is required if you would like to tinker with the usb_hid_device firmware.

```
sudo apt-get install gforth
```

### Perl

Perl is required if you would like to tinker with the usb_hid_host firmware. Your system most likely already has Perl installed. If not:

```
sudo apt-get install perl.
```

## Additional Prerequisites for Automated Testing on FPGA

## Screen

The Screen utility is used for automated serial port interaction in *lisp_test.py*.

```
sudo apt-get install screen.
```

## Hidden Tool Dependencies

The following tools are automatically installed (if not already installed) by sourcing the `activate_env.sh` script:

- [OSS CAD Suite (CoCoTB, iverilog, GTKWave, openFPGALoader, OpenOCD, Python)](https://github.com/YosysHQ/oss-cad-suite-build)
- [Bender](https://github.com/pulp-platform/bender)
- Python packages NumPy, SciPy, Matplotlib, Corsair, Pyte.
- [BoxLambda fork of Litex](https://github.com/epsilon537/litex)
- **riscv32-boxlambda-elf**: RISCV32 GNU toolchain custom-built for BoxLambda.

