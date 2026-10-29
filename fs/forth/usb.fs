\ BoxLambda Forth
\ USB HID dispatcher

hook-os-timer @ variable (usb-prev-hook-timer)

0 variable usb-os-ticks

: (usb-os-tick)
  1 usb-os-ticks +!
  (usb-prev-hook-timer) @ execute
;

' (usb-os-tick) hook-os-timer !

0 variable keyboard-report-count

: usb-hid-0-irq-handle
  USB_HID_0_ISR_USB_REPORT@ 
  USB_HID_STATUS_USB_TYP@ USB_HID_STATUS_USB_TYP_KEYB = 
  and if
    keyboard-report-count 1+
  then
;

' usb-hid-0-irq-handle irq-usb-hid-0

1 irq-id-usb-hid-0 lshift mie!

: wait-for-keyboard
  begin
    USB_HID_0_STATUS_ADDR @ USB_HID_0_STATUS_USB_TYP_KEYB =
  until
;

." Waiting for keyboard ... " cr
\ wait-for-keyboard
\ ." Keyboard detected" cr

