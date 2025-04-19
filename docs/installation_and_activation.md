---
hide:
  - toc
---

## Installation

To install BoxLambda:

1. [Install the prerequisites](prerequisites.md).
2. [Hook up the peripherals (PMODs)](pmods.md).
3. [Get the repository](#getting-the-repository).
4. [Set up the environment](#setting-up-the-environment).
5. [Activate the environment](#activating-the-tools-environment).

The first four steps need to be executed once. Activating the environment is required every time you're working with BoxLambda.

### Getting the Repository

```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```

### Setting Up the Environment

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

The script is easy to follow. The tools versions used are specified inside the script.

If you want to force a reinstall of the tools, delete the `./tools/` directory and source the `boxlambda_setup.sh` script again.

### User-Level Access to the Arty A7 USB JTAG Adapter.

When run at the user level, OpenOCD might not have permission to access the USB JTAG adapter. To fix this issue, add a rule to `/etc/udev/rules.d`.
Create a file named `99-openocd.rules` in the `/etc/udev/rules.d` directory. This file should have the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

### Serial Port
Your mileage may vary but on my Ubuntu system, when I plug in my Arty A7, the serial port shows up at `/dev/ttyUSB1`.

Serial Port Settings: **115200 8N1**

## Activating the Tools Environment

When working with BoxLambda, its tools environment needs to be activated. This is done by sourcing the `activate_env.sh` script.

```
source activate_env.sh
```

This script will:

- Check if the essential prerequisites (Vivado, RISC-V compiler) are installed.
- Activate the OSS CAD Suite environment. This includes a Python environment.
- Update the PATH environment variable to include the required tools.

If you have multiple BoxLambda workspaces, you only need to activate the tools environment in one of them.

To deactivate the environment again, enter `deactivate`.

