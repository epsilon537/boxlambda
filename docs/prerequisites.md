## Prerequisites

- **Host OS**: Linux or Linux WSL.
  
- **Vivado ML** Edition V2023.1, Linux version:
  
    [https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2023-1.html](https://www.xilinx.com/support/download/index.html/content/xilinx/en/downloadNav/vivado-design-tools/2023-1.html)
    
    Make sure you also install your Arty A7 board files. Digilent has excellent instructions for installing Vivado and Digilent board files:
    
    [https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis](https://digilent.com/reference/programmable-logic/guides/installing-vivado-and-vitis)

- RISCV Compiler Toolchain **rv32imcb**. This is the cross-compiler for building the code that'll run on the Ibex processor. I'm using the **20220210-1** pre-built binaries from *lowRISC*:
   
	[https://github.com/lowRISC/lowrisc-toolchains/releases](https://github.com/lowRISC/lowrisc-toolchains/releases)

    Add the toolchain's *bin/* directory to your *PATH*. E.g.:

```
    export RISCV_TOOLCHAIN=$HOME/lowrisc-toolchain-gcc-rv32imcb-20220210-1
    export PATH=$PATH:$RISCV_TOOLCHAIN/bin
```

- **GNU Make** version 4.2.1: 
  
    [https://www.gnu.org/software/make/](https://www.gnu.org/software/make/)
    
    Please make sure make is in your *PATH*.
  
- **Bender** 0.27.1: 
  
    [https://github.com/pulp-platform/bender/releases/tag/v0.27.1](https://github.com/pulp-platform/bender/releases/tag/v0.27.1)
    
    Add bender to your *PATH*.

- **Verilator** 4.216: 
  
    [https://verilator.org/guide/latest/install.html](https://verilator.org/guide/latest/install.html)

    Add verilator to your *PATH*.

- **Ncurses**:
  
```
    sudo apt-get install libncurses5-dev libncursesw5-dev libncursesw5
```

- **Gtkwave**: 
  
    [http://gtkwave.sourceforge.net/](http://gtkwave.sourceforge.net/)

- **RISCV OpenOCD**

    Build RISCV OpenOCD from source:
    
```
      git clone https://github.com/riscv/riscv-openocd
      cd riscv-openocd
      git submodule update --init --recursive
      ./bootstrap
      ./configure --disable-werror --disable-wextra --enable-remote-bitbang --enable-ftdi
      make
      sudo make install
``` 
      Add the install directory (*/usr/local/bin* in my case) to your PATH.  

- **LiteX**: The installation instructions are on the LiteX Wiki:

    [https://github.com/enjoy-digital/litex/wiki/Installation](https://github.com/enjoy-digital/litex/wiki/Installation)

- **CMake**: Version 3.24 or later.
  
    On Ubuntu, follow these instructions: [https://apt.kitware.com/](https://apt.kitware.com/)
  
    On other distros, please use your distro's package installer to get version 3.23 or later.

    Make sure that the correct CMake version is first in your PATH. The Vivado */tools/Xilinx/Vivado/2023.1/settings64.sh* script adds an old version of CMake to your path. I override that by re-adding */usr/bin* to the front of the PATH after sourcing the Vivado script:

```
    source /tools/Xilinx/Vivado/2023.1/settings64.sh
    export PATH=/usr/bin:$PATH
```

- **SDL2**: [https://wiki.libsdl.org/SDL2/Installation](https://wiki.libsdl.org/SDL2/Installation)
  
    Also, make sure to set environment variable **SDL2_DIR** to point to the SDL2 directory containing files *SDL2Config.cmake* or *sdl2-config.cmake*. In my case (Ubuntu WSL), I added the following line to my *~/.bashrc*:

    `export SDL2_DIR=/usr/lib/x86_64-linux-gnu/cmake/SDL2/`
