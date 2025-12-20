# USB HID Keyboard and Mouse Test

The USB HID System Test Case, running both on Verilator and on FPGA, continuously polls the two USB cores for report events. Whenever there's a report event (indicated in the `USB_HID_ISR` register), the device type (Keyboard/Mouse) and report details (mouse movement, keypress...) are printed.

Additionally, when Switch 0 (SW0) is set to *On* and a USB keyboard is connected, the keyboard LEDs will be turned on and off in a rotating pattern.

[https://github.com/epsilon537/boxlambda/blob/master/sw/projects/usb_hid_sys_test/usb_hid_sys_test.c](https://github.com/epsilon537/boxlambda/blob/master/sw/projects/usb_hid_sys_test/usb_hid_sys_test.c)

## The USB HID Test on Verilator

Build the *usb_hid_sys_test* project:

```
cd build/sim-a7-100/gw/projects/usb_hid_sys_test
make usb_hid_sys_test_sim
```

Execute the generated Verilator model. You should see the following output (it'll take a minute before you start seeing any output):

```
./Vmodel
...
USB HID Test Start.
USB1: Status change: 0x0 -> 0x1
  Keyboard detected.
ledg_1 = 2
USB1 keyboard report: 0x210900001000100
  Key mods: 0x0 Keys: 0x0
USB0: Status change: 0x0 -> 0x2
  Mouse detected.
USB0 mouse report: 0xcffcfc001000100
  Mouse: 0x0
USB1 keyboard report: 0x400000
  Key mods: 0x0 Keys: 0x4
USB0 mouse report: 0xcffcfc001000100
  Mouse: 0x0
...
Test passed.
```

The `ledg_1 = ...` lines indicate a LED update in the emulated USB keyboard.

## The USB HID Test on FPGA

Hook up Machdyne's [USB host dual socket PMOD](https://machdyne.com/product/usb-host-dual-socket-pmod/) to port `JA` and connect a keyboard and/or a mouse.

Connect a terminal emulator such as Putty or Minicom to Arty's USB serial port. **Settings: 1000000 8N1**.

Build the project in an Arty A7 build tree:

```
cd build/arty-a7-100/gw/projects/usb_hid_sys_test
make usb_hid_sys_test_bit
```

Download the generated bitstream file to the Arty A7:

```
make usb_hid_sys_test_load
```

Make sure Switch 0 (`SW0`) is in the *Off* position (flipped toward the edge of the board).

Press some keys on the keyboard, and move the mouse around. You should see the results in the Putty terminal.

Flip `SW0` on.

You should now see the keyboard LEDs rotate.

