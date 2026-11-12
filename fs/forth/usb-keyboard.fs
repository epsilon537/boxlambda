\ BoxLambda Forth
\ USB HID dispatcher

100 constant USB-KEYBOARD-DETECT-PERIOD

\ This variable if a USB keyboard is connected.
false variable usb-keyboard-present

hook-os-timer @ variable (usb-prev-hook-timer)
0 variable (usb-keyboard-detect-ticks)
0 variable (usb-keyboard-report-count)

50 variable usb-keyboard-repeat-start-ticks
4 variable usb-keyboard-repeat-period-ticks
0 variable (usb-keyboard-repeat-ticks)
-1 variable (usb-keyboard-repeat-ticks-cmp)

\ Enable/disable software test mode
( f -- )
: usb-keyboard-sw-test-mode USB_HID_0_STATUS_BLOCK_REPORTS! ;

\ Returns software test mode
( -- f )
: usb-keyboard-sw-test-mode? USB_HID_0_STATUS_BLOCK_REPORTS@ 0<> ;

( -- )
: (usb-keyboard-detect)
  (usb-keyboard-detect-ticks) @ 1+ ( ticks )
  dup USB-KEYBOARD-DETECT-PERIOD >= if ( ticks )
    drop 0 ( ticks)
    (usb-keyboard-report-count) @ 0> ( ticks f )
    dup usb-keyboard-present ! ( ticks f )
    dup led-keyboard-detect! 0= ( ticks f )
    usb-keyboard-sw-test-mode? not and ( ticks f )
    \ Reset the core if keyboard is no longer present to allow re-detect/re-enumeration.
    if
      RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL!
    then ( ticks )
    0 (usb-keyboard-report-count) ! ( ticks )
  then
  (usb-keyboard-detect-ticks) ! ( )
;

0 variable (prev-keypress) \ The previous keys-pressed register value.
0 variable (new-keypress)  \ The current keys-pressed register value.
0 variable (prev-keymods) \ The previous USB_HID_0_KEY_MODS register value. Left/right folded.
0 variable (new-keymods) \ The current USB_HID_0_KEY_MODS register value. Left/right folded.

\ keymods bitmask values
$1 constant KEYMOD-CTRL
$2 constant KEYMOD-SHIFT
$4 constant KEYMOD-ALT
$8 constant KEYMOD-WIN

\ Read keymods register and fold left/right.
( -- keymods )
: (read-keymods-fold)
  USB_HID_0_KEY_MODS_ADDR @ dup 4 rshift or $f and
;

\ Read keys-pressed register.
( -- keypress )
: (read-keypress)
  USB_HID_0_KEYS_ADDR @ ( keys-pressed )
;

( -- )
: (update-keymods)
  (new-keymods) @ (prev-keymods) !
  (read-keymods-fold) (new-keymods) !
;

( -- )
: (update-keypress)
  (new-keypress) @ (prev-keypress) !
  (read-keypress) (new-keypress) !
;

begin-structure key-event-struct-raw
  cfield: .scancode
  cfield: .modifiers
  cfield: .upndown
  cfield: .repeat
end-structure

create key-event-raw key-event-struct-raw allot

( -- )
: .key-event
  key-event-raw .modifiers c@ key-event-raw .scancode c@
  s" Raw key event: scancode: $%x modifiers: $%x " printf
  key-event-raw .upndown c@ if ." up " else ." down " then
  key-event-raw .repeat c@ if ." repeat" then
  cr
;

' .key-event variable key-event-raw-hook

( scancode upndown repeat -- )
: (report-key-event)
  -rot ( repeat scancode upndown )
  swap key-event-raw .scancode c! ( repeat upndown )
  dup if (prev-keymods) else (new-keymods) then @ ( repeat upndown keymods )
  key-event-raw .modifiers c! ( repeat upndown )
  key-event-raw .upndown c! ( repeat )
  key-event-raw .repeat c! ( )
  key-event-raw-hook @ execute ( )
;

\ Returns true if byte is found in at least one of the byte positions in word.
( byte word -- f )
: find-byte-in-word
  4 0 do ( byte word )
    2dup $ff and = if ( byte word )
      2drop
      unloop true exit
    then
    8 rshift ( byte word' )
  loop
  2drop ( )
  false
;

( check against upndown -- )
: (process-keychanges)
  >r ( check against R: upndown )
  >r ( check R: upndown against )
  begin
    dup while ( check R: upndown against )
      dup $ff and ( check key R: upndown against )
      dup r@ find-byte-in-word 0= if ( check key R: upndown against )
        \ mismatch -> key-event.
        dup 1 rpick false ( key upndown repeat )
        (report-key-event) ( check key R: upndown against )
      then
      drop 8 rshift ( check' R: upndown against )
  repeat ( check R: upndown against )
  drop ( R: upndown against )
  2rdrop ( )
;

( -- )
: (process-keyups) \ Are any of the old keys no longer present in the new key-presses?
  (prev-keypress) @ ( prev-keys )
  (new-keypress) @ ( prev-keys new-keys )
  true (process-keychanges) ( )
;

( -- )
: (process-keydns) \ Are any of the new keys not yet present in the old key-presses?
  (new-keypress) @ ( new-keys )
  (prev-keypress) @ ( new-keys prev-keys )
  false (process-keychanges) ( )
;

( v -- )
: (usb-keyboard-repeat-timer-start)
  (usb-keyboard-repeat-ticks-cmp) !
  0 (usb-keyboard-repeat-ticks) ! \ Restart the timer
;

: (usb-keyboard-repeat-timer-stop)
  -1 (usb-keyboard-repeat-ticks-cmp) !
;

( -- )
: (process-keyreport)
  (update-keymods)
  (update-keypress)
  (new-keypress) @ (prev-keypress) @ <> if \ any key-press changes?
    (process-keyups)
    (process-keydns)
    (new-keypress) @ if \ any keys pressed? -> (re)start the repeat timer.
      usb-keyboard-repeat-start-ticks @ (usb-keyboard-repeat-timer-start)
    else \ no keys pressed currently -> cancel repeat timer
      (usb-keyboard-repeat-timer-stop)
    then
  then
;

: (usb-hid-0-irq-handle)
  USB_HID_0_ISR_USB_REPORT@ if
    \ Acknowledge the USB report interrupt in the USB core.
    1 USB_HID_0_ISR_USB_REPORT!
    USB_HID_0_STATUS_USB_TYP@ USB_HID_0_STATUS_USB_TYP_KEYB = if
      1 (usb-keyboard-report-count) +!
    then
    (process-keyreport)
  then
;

' (usb-hid-0-irq-handle) irq-usb_hid_0 !

0 variable (usb-keyboard-no-repeat-keys)

( k0 k1 k2 k3 -- )
\ Specify 4 scancodes that won't repeat on long-press. 0 is dummy value.
: usb-keyboard-no-repeat-keys!
  0 ( k0 k1 k2 k3 acc )
  4 0 do ( k0..kn acc )
    swap ?dup if ( k0..kn-1 acc kn ) \ non-zero keys only.
      swap 8 lshift ( k0..kn acc' )
      or ( k0..kn-1 acc )
    then
  loop
  (usb-keyboard-no-repeat-keys) !
;

( -- k0 k1 k2 k3 )
: usb-keyboard-no-repeat-keys@
  4 0 do
    (usb-keyboard-no-repeat-keys) @ ( k0..kn-1 acc )
    i 8 * rshift $ff and ( k0 .. kn )
  loop
;

( -- )
: (report-keypress-repeat)
  (new-keypress) @
  begin ( keypresses )
    dup while ( keypresses )
      dup $ff and ( keypresses key )
      dup (usb-keyboard-no-repeat-keys) @ find-byte-in-word not if ( keypresses key )
        dup false true (report-key-event) ( keypresses key )
      then
      drop
      8 rshift ( keypresses )
  repeat
  drop ( )
;

: (usb-keyboard-repeat)
  (usb-keyboard-repeat-ticks) @ 1+ ( ticks )
  dup (usb-keyboard-repeat-ticks) ! ( ticks )
  (usb-keyboard-repeat-ticks-cmp) @ u>= if ( )
    usb-keyboard-repeat-period-ticks @ (usb-keyboard-repeat-timer-start)
    (report-keypress-repeat)
  then
;

( -- )
: (usb-keyboard-os-tick)
  (usb-keyboard-detect)
  (usb-keyboard-repeat)
  (usb-prev-hook-timer) @ execute
;

( -- )
: usb-keyboard-start
  \ Reset the core.
  RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL!
  \ Clear the USB report interrupt in the USB core.
  1 USB_HID_0_ISR_USB_REPORT!
  1 USB_HID_0_IEN_USB_REPORT!
  mie irq-id-usb-hid-0 1<< or mie!
  ['] (usb-keyboard-os-tick) hook-os-timer !
;

( -- )
: usb-keyboard-stop
  0 USB_HID_0_IEN_USB_REPORT!
  mie irq-id-usb-hid-0 1<< bic mie!
  ['] (usb-prev-hook-timer) hook-os-timer !
;

