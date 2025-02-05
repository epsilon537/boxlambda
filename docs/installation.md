## Installation

To install BoxLambda:

1. [Get the repository](#getting-the-repository).
2. [Activate the tools environment](#activating-the-tools-environment).
3. [Install the prerequisites](prerequisites.md).
4. [Hook up the peripherals (PMODs)](pmods.md).
5. [Initialize the repository and create the build trees](#initializing-the-repository-and-creating-build-trees).

### Getting the Repository

```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```

### Activating the Tools Environment

From the root of the BoxLambda workspace, source the *activate_env.sh* script:
```
source activate_env.sh
```

This script will:

- Install the required tools (compiler, oss-cad-suite, Bender...) if not already installed.
- Activate the OSS CAD Suite environment. This includes a Python environment.
- Install the required Python packages if not already installed.
- Update the PATH environment variable to include the required tools.

The script is easy to follow. The tools versions used are specified inside the script.

If you have multiple BoxLambda workspaces, you only need to activate the tools environment in one of them.

If you want to force a reinstall of the tools, source the script with the `-r` option.

To deactivate the environment again, enter `deactivate`.

### Initializing the Repository and Creating Build Trees.

The following script initializes the git submodules used and creates the default build trees:

```
./boxlambda_setup.sh
```

### User-Level Access to the Arty A7 USB JTAG Adapter.

When run at the user level, OpenOCD might not have permission to access the USB JTAG adapter. To fix this issue, add a rule to **/etc/udev/rules.d**.
Create a file named **99-openocd.rules** in the **/etc/udev/rules.d** directory. This file should have the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

### Serial Port
Your mileage may vary but on my Ubuntu system, when I plug in my Arty A7, the serial port shows up at **/dev/ttyUSB1**.

Serial Port Settings: **115200 8N1**

### On WSL

#### USBIPD-WIN

For USB device access to work on WSL, it's necessary to attach the USB device to WSL (by default, USB ports stay under native Windows control). This is done using **usbipd-win**, which can be installed from this location:

[https://github.com/dorssel/usbipd-win/releases](https://github.com/dorssel/usbipd-win/releases).

Additional info about connecting USB devices to WSL can be found here:

[https://learn.microsoft.com/en-us/windows/wsl/connect-usb](https://learn.microsoft.com/en-us/windows/wsl/connect-usb).

For convenience, I created a one-line Windows batch script that attaches the Arty USB JTAG port to WSL:

*boxlambda/scripts/usb_fwd_to_wsl.bat*:

```
usbipd attach -i 0403:6010 -a -w
```

#### Udev

On Ubuntu WSL, *udev*, the system service enforcing device permissions, isn't running by default. To fix this, add the following lines to **/etc/wsl.conf**:

```
[boot]
command="service udev start"
```

Without *udev* running, OpenOCD or Vivado will not have access to the Arty USB JTAG adapter when executed at the user level.
