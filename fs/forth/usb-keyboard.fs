\ BoxLambda Forth
\ USB HID dispatcher

hook-os-timer @ variable (usb-prev-hook-timer)

0 variable usb-os-ticks

10 constant USB-KEYBOARD-DETECT-PERIOD

0 variable usb-keyboard-report-count
false variable usb-keyboard-present

: (usb-os-tick)
  usb-os-ticks @ 1+ ( ticks )
  dup USB-KEYBOARD-DETECT-PERIOD >= if ( ticks )
    drop 0 ( ticks)
    usb-keyboard-report-count @ 0> usb-keyboard-present ! ( ticks )
    0 usb-keyboard-report-count ! ( ticks )
  then
  usb-os-ticks !
  (usb-prev-hook-timer) @ execute
;

' (usb-os-tick) hook-os-timer !

: (usb-hid-0-irq-handle)
  USB_HID_0_ISR_USB_REPORT@ if 
    \ Acknowledge the USB report interrupt in the USB core.
    1 USB_HID_0_ISR_USB_REPORT!
    USB_HID_0_STATUS_USB_TYP@ USB_HID_0_STATUS_USB_TYP_KEYB = if
      1 usb-keyboard-report-count +!
    then
  then
;

' (usb-hid-0-irq-handle) irq-usb_hid_0 !

( -- )
: usb-keyboard-start
  \ Clear the USB report interrupt in the USB core.
  1 USB_HID_0_ISR_USB_REPORT!
  1 USB_HID_0_IEN_USB_REPORT!
  mie irq-id-usb-hid-0 1<< or mie!
;

( -- )
: usb-keyboard-stop
  0 USB_HID_0_IEN_USB_REPORT!
  mie irq-id-usb-hid-0 1<< bic mie!
;
: wait-for-keyboard
  begin
    USB_HID_0_STATUS_ADDR @ USB_HID_0_STATUS_USB_TYP_KEYB =
  until
;

." Waiting for keyboard ... " cr
\ wait-for-keyboard
\ ." Keyboard detected" cr

