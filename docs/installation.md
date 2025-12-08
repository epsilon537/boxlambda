# BoxLambda Installation

To install BoxLambda:

1. [Install the prerequisites](prerequisites.md).
2. [Hook up the peripherals (PMODs)](pmods.md).
3. [Get the repository](#getting-the-repository).
4. [Set up the environment](#setting-up-the-environment).
5. [Activate the environment](#activating-the-tools-environment).
6. [Flash the bitstream, bootloader, and OS binaries](#installing-the-boxlambda-base-bitstream-bootloader-and-os).

The first four steps need to be executed once. Activating the environment is required every time you're working with BoxLambda.

## Getting the Repository

```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```

## Setting Up the Environment

The following script initializes the git submodules used and creates the default build trees:

```
source boxlambda_setup.sh
```

This script will:

- Initialize the git submodules.
- Install the required tools (compiler, oss-cad-suite, Bender...) if not already installed.
- Activate the OSS CAD Suite environment. This includes a Python environment.
- Install the required Python packages if not already installed.
- Update the PATH environment variable to include the required tools.
- Create the default build trees.

The script is easy to follow. The tool versions used are specified inside the script.

If you want to force a reinstall of the tools, delete the `./tools/` directory and source the `boxlambda_setup.sh` script again.

### User-Level Access to the Arty A7 USB JTAG Adapter.

When run at the user level, OpenOCD might not have permission to access the USB JTAG adapter. To fix this issue, add a rule to `/etc/udev/rules.d`.
Create a file named `99-openocd.rules` in the `/etc/udev/rules.d` directory. This file should have the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

### Serial Port

On my Linux system, when I plug in my Arty A7, the serial port shows up at `/dev/ttyUSB1`.

Serial Port Settings: **1000000 8N1**

## Activating the Tools Environment

When working with BoxLambda, its tools environment has to be activated. This is done by sourcing the `activate_env.sh` script.

```
source activate_env.sh
```

This script will:

- Check if the essential prerequisites (Vivado, RISC-V compiler) are installed.
- Activate the OSS CAD Suite environment. This includes a Python environment.
- Update the PATH environment variable to include the required tools.

If you have multiple BoxLambda workspaces, you only need to activate the tools environment in one of them.

To deactivate the environment again, enter `deactivate`.

### Installing the BoxLambda Base Bitstream, BootLoader, and OS

#### Flashing the Release Binaries

The BoxLambda repo contains a `binaries/` subdirectory with release binaries. To flash those to the target:

Connect a terminal emulator to the Arty's USB serial port. **Settings: 1000000 8N1**.

Set one of the board switches to *On*. It doesn't matter which one.

From the repo root directory, navigate to the `binaries` subdirectory and flash the release bootloader image, the OS image, and the release bitstream image:

```
cd binaries
flash_sw.sh arty_a7_100t -b bootloader.bin
flash_sw.sh arty_a7_100t boxlambda_os.bin
flash_gw.sh arty_a7_100t boxlambda_base.bit
```

Note the `-b` flag when flashing the bootloader binary, indicating to the `flash_sw.sh` script to write to the flash memory region set aside for the bootloader.

Once flashing is complete, the target will be reset. In the terminal, you should first see the bootloader image, then the OS image booting up:

```
BoxLambda bootloader
--------------------
Version: v0.3.0
Initializing SDRAM...
Initializing SDRAM @0x20000000...
Switching SDRAM to software control.
Read leveling:
  m0, b00: |00000000000000000000000000000000| delays: -
  m0, b01: |11111111111111111111111111111000| delays: 14+-14
  m0, b02: |00000000000000000000000000000000| delays: -
  m0, b03: |00000000000000000000000000000000| delays: -
  m0, b04: |00000000000000000000000000000000| delays: -
  m0, b05: |00000000000000000000000000000000| delays: -
  m0, b06: |00000000000000000000000000000000| delays: -
  m0, b07: |00000000000000000000000000000000| delays: -
  best: m0, b01 delays: 14+-14
  m1, b00: |00000000000000000000000000000000| delays: -
  m1, b01: |11111111111111111111111111111100| delays: 14+-14
  m1, b02: |00000000000000000000000000000000| delays: -
  m1, b03: |00000000000000000000000000000000| delays: -
  m1, b04: |00000000000000000000000000000000| delays: -
  m1, b05: |00000000000000000000000000000000| delays: -
  m1, b06: |00000000000000000000000000000000| delays: -
  m1, b07: |00000000000000000000000000000000| delays: -
  best: m1, b01 delays: 14+-14
Switching SDRAM to hardware control.
Done.
Done.
Application image magic number check OK.
Application image size: 77364 bytes
Copying SW image from Flash to DDR...
Done.
Booting application image...
Initializing Forth.

Mecrisp-Quintus 1.1.1 for RISC-V RV32IM by Matthias Koch.
BoxLambda port by Ruben Lysens/Epsilon537.
Forth core init complete.
Compiling Forth included_tools...
Redefine forget. Redefine u.4. Redefine u.2.

     _
    ^-)
     (.._          .._
      \`\\        (\`\\        (
       |>         ) |>        |)
______/|________ (7 |` ______\|/_______a:f

```

You're now in the Forth REPL and can start entering Forth statements.

#### Building and Flashing from Source

If you prefer, you can build the BoxLambda Base bitstream, the Bootloader and the OS software image from source. From the repo root directory:

```
cd build/arty-a7-100/sw/projects/bootloader
make bootloader
make bootloader_flash_sw
```

Again from the repo root directory:

```
cd build/arty-a7-100/sw/projects/boxlambda_os
make bootloader
make bootloader_flash_sw
```

And once more from the repo root directory:

```
cd build/arty-a7-100/gw/projects/boxlambda_base
make boxlambda_base.bit
make boxlambda_base_flash_gw
```

The `make boxlambda_base.bit` step will take a few minutes to complete. The other build steps should complete in seconds.

You should see the same output on the serial port terminal as shown in the previous section.

