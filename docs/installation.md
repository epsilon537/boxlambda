## Installation

Before you try any of the test builds, you need to:

1. [Install the Prerequisites](prerequisites.md).
2. [Hook Up the Peripherals (PMODs)](pmods.md).
3. [Set up the repository](#setting-up-the-repository).

### Setting Up the Repository.

Get the BoxLambda repository:
```
git clone https://github.com/epsilon537/boxlambda/
cd boxlambda
```
Switch to the **interconnect** tag: 
```
git checkout interconnect
```

Set up the repository. This initializes the git submodules used and creates the default build trees: 
```
./boxlambda_setup.sh
```

### User-Level Access to the Arty A7 USB JTAG Adapter.

OpenOCD might not have permission to access the USB JTAG adapter when run at the user level. To fix this issue, you need to add a rule to **/etc/udev/rules.d**.
Create a file with the name **99-openocd.rules** in the **/etc/udev/rules.d** directory. This file should have the following contents:

```
# Original FT2232 VID:PID
SUBSYSTEM=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="666", GROUP="plugdev"

```

### On WSL

#### USBIPD-WIN

For USB device access to work at all on WSL, it's necessary to attach the USB device to WSL (by default, USB ports stay under native Windows control). This is done using **usbipd-win**, which can be installed from this location: 

[https://github.com/dorssel/usbipd-win/releases](https://github.com/dorssel/usbipd-win/releases).

Additional info about connecting USB devices to WSL can be found here: 

[https://learn.microsoft.com/en-us/windows/wsl/connect-usb](https://learn.microsoft.com/en-us/windows/wsl/connect-usb).

For convenience, I created a one-line Windows batch script that attaches the Arty USB JTAG port to WSL: 

*boxlambda/scripts/usb_fwd_to_wsl.bat*:

```
usbipd attach -i 0403:6010 -a -w
```

#### Udev

On Ubuntu WSL, *udev*, the system service in charge of enforcing device permissions, isn't running by default. To fix this, add the following lines to **/etc/wsl.conf**:

```
[boot]
command="service udev start"
```

Without *udev* running, OpenOCD or Vivado will not have access to the Arty USB JTAG adapter when executed at the user level.
