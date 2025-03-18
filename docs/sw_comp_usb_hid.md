---
hide:
  - toc
---

## USB_HID_Host Hardware Access Layer

**Usb_hid_host HAL**: [sw/components/usb_hid/usb_hid_hal.h](https://github.com/epsilon537/boxlambda/blob/master/sw/components/usb_hid/usb_hid_hal.h)

The USB HID HAL is a very thin Hardware Access Layer, mapping directly to the two USB cores' registers.

## The usb_hid_host firmware

The usb_hid_host UKP firmware *.hex* image is checked in, so you don't need to build it from source.

UKP Firmware directory: [sub/usb_hid_host/src/usb_hid_host/](https://github.com/epsilon537/usb_hid_host/tree/boxlambda/src/usb_hid_host)

After making your changes, run the *asukp* script in that same directory and a new firmware image will be generated.

## The usb_hid_device firmware

The usb_hid_device firmware *.hex* images for the emulated keyboard and mouse are checked in, so you don't need to build them from source.

The firmware is located here:

- Mouse Firmware directory: [sub/usb_hid_device/firmware_mouse/](https://github.com/epsilon537/usb_hid_device/tree/boxlambda/firmware_mouse)
- Keyboard Firmware directory: [sub/usb_hid_device/firmware_keyboard/](https://github.com/epsilon537/usb_hid_device/tree/boxlambda/firmware_keyboard)

After making your changes, just run *make* in that same directory and a new firmware image will be generated.


