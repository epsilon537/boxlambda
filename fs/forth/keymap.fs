\ BoxLambda Forth
\ USB HID dispatcher

10 constant USB-KEYBOARD-DETECT-PERIOD

false variable usb-keyboard-present

hook-os-timer @ variable (usb-prev-hook-timer)
0 variable (usb-keyboard-detect-ticks)
0 variable (usb-keyboard-report-count)

: (keyboard-detect)
  (usb-keyboard-detect-ticks) @ 1+ ( ticks )
  dup USB-KEYBOARD-DETECT-PERIOD >= if ( ticks )
    drop 0 ( ticks)
    (usb-keyboard-report-count) @ 0> ( ticks f )
    dup usb-keyboard-present ! ( ticks f )
    dup led-keyboard-detect! ( ticks f )
    \ Reset the core if keyboard is no longer present to allow re-detect/re-enumeration.
    0= if RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL! then ( ticks )
    0 (usb-keyboard-report-count) ! ( ticks )
  then
  (usb-keyboard-detect-ticks) !
;

0 variable prev-keydn
0 variable new-keydn
0 variable prev-keymods
0 variable new-keymods
0 variable sticky-keymods

$2C constant SCANCODE-SPC

$1 constant KEYMOD-CTRL
$2 constant KEYMOD-SHIFT
$4 constant KEYMOD-ALT
$8 constant KEYMOD-WIN
$10 constant KEYMOD-SPC

( -- keymods )
: (get-keymods-ctrl-shft-alt)
  USB_HID_0_KEY_MODS_ADDR @ dup 4 rshift or $f and
;

( keys -- keys' )
: (update-keymods)
  new-keymods @ prev-keymods ! ( keys ) 
  (get-keymods-ctrl-shft-alt) ( keys ctrl-shft-alt )
  over $ff and SCANCODE-SPC = if ( keys ctrl-shft-alt )
    KEYMOD-SPC or ( keys keymods )
    swap 8 rshift swap ( keys' keymods )
  then
  new-keymods ! ( keys' )
;

( keys -- keys' )
: (update-keydn)
  new-keydn @ prev-keydn ! ( keys ) 
  dup $ff and new-keydn ! ( keys )
  8 rshift ( keys' )
;

begin-structure key-event-struct
  cfield: .scancode
  cfield: .keycode
  cfield: .modifiers
  cfield: .upndown
  cfield: .repeat
end-structure

16 constant KEY-EVENT-RING-NUM-ENTRIES
create key-event-ring key-event-struct KEY-EVENT-RING-NUM-ENTRIES * allot
key-event-ring variable key-event-ring-wr
key-event-ring variable key-event-ring-rd
key-event-ring key-event-struct KEY-EVENT-RING-NUM-ENTRIES 1- * + constant KEY-EVENT-RING-LAST

( keymods -- offset )
: (keymods>offset)
  dup case
    KEYMOD-SPC and ?of 2
    KEYMOD-ALT and ?of 4
    0
  endcase ( keymods offset )
  swap KEYMOD-SHIFT and if
    1+
  then ( offset )
;

: keycode-gen ( alt-shft alt spc-shft spc shft vanilla "name" -- )
  create
    \ compile-time: store the parameters in the dictionary body
    \ vanilla, shift, spc, spc-shift, alt, alt-shft
    char c, char c, char c, char c, char c, char c,
  does> ( keymods pfa -- keycode )
    swap (keymods>offset) ( pfa offset )
    + c@ ( keycode )
;

keycode-gen keycode-gen-q [ [ P p Q q

( scancode -- keycode )
: (scan>keycode)
;

( -- )
: (key-event-ring-wr-advance)
  key-event-ring-wr @
  KEY-EVENT-RING-LAST < if ( wrptr )
    key-event-struct + ( wrptr' )
  else
    drop key-event-ring ( wrptr' )
  then
  key-event-ring-wr ! ( )
;

( scancode -- )
: (key-up-event)
  key-event-ring-wr @ ( scancode wrptr )
  2dup .scancode c! ( scancode wrptr )
  dup false swap .repeat c! ( scancode wrptr )
  dup new-keymods @ swap .modifiers c! ( scancode wrptr )
  dup true swap .upndown c! ( scancode wrptr )
  swap (scan>keycode) swap .keycode c! ( )
  (key-event-ring-wr-advance)
;

( scancode -- )
: (key-down-event)
;

( -- )
: (space-key-event)
;

( keymods -- f)
: keymods-spc? KEYMOD-SPC and 0<> ;

( -- )
: (process-keyspcdn)
  (new-keydn) @ (prev-keydn) @ ( new prev )
  2dup = if ( new prev ) \ no changes in the regular keys.
    2drop
    new-keymods @ keymods-spc? not 
    prev-keymods @ keymods-spc? and if \ space key released.
      (space-key-event)
    then
  else ( new prev ) \ a key was pressed or released.
    ?dup if \ the previous key was released.
      dup (key-up-event)
    then ( new )
    if \ a new key was pressed.
      (key-down-event)
    then ( )
  then
;

( -- )
: (usb-keyboard-os-tick)
  (keyboard-detect)
  drop
  (usb-prev-hook-timer) @ execute
;

' (usb-keyboard-os-tick) hook-os-timer !

: (usb-hid-0-irq-handle)
  USB_HID_0_ISR_USB_REPORT@ if
    \ Acknowledge the USB report interrupt in the USB core.
    1 USB_HID_0_ISR_USB_REPORT!
    USB_HID_0_STATUS_USB_TYP@ USB_HID_0_STATUS_USB_TYP_KEYB = if
      1 (usb-keyboard-report-count) +!
    then
    USB_HID_0_KEYS_ADDR @ ( keys )
    (update-keymods) ( keys' )
    (update-keydn) ( keys' )
    drop
    (process-keyspcdn) 
  then
;

' (usb-hid-0-irq-handle) irq-usb_hid_0 !

( -- )
: usb-keyboard-start
  \ Reset the core.
  RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL!
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

$1E constant SCANCODE-1
$1F constant SCANCODE-2
$20 constant SCANCODE-3
$21 constant SCANCODE-4
$22 constant SCANCODE-5

$2B constant SCANCODE-TAB
$30 constant SCANCODE-CAPS

$14 constant SCANCODE-Q
$1A constant SCANCODE-W
$08 constant SCANCODE-E
$15 constant SCANCODE-R
$17 constant SCANCODE-T

$04 constant SCANCODE-A
$16 constant SCANCODE-S
$07 constant SCANCODE-D
$08 constant SCANCODE-F
$0A constant SCANCODE-G

$1D constant SCANCODE-Z
$1B constant SCANCODE-X
$0C constant SCANCODE-C
$19 constant SCANCODE-V
$05 constnat SCANCODE-B

0 constant KEY-ENTER
0 constant KEY-ESCAPE
0 constant KEY-BACKSPACE
0 constant KEY-TAB
0 constant KEY-DEL
0 constant KEY-HOME
0 constant KEY-END
0 constant KEY-PGUP
0 constant KEY-PGDN
0 constant KEY-UP
0 constant KEY-DN
0 constant KEY-LEFT
0 constant KEY-RIGHT
0 constant KEY-F1
0 constant KEY-F2
0 constant KEY-F3
0 constant KEY-F4
0 constant KEY-F5
0 constant KEY-F6
0 constant KEY-F7
0 constant KEY-F8
0 constant KEY-F9
0 constant KEY-F10
0 constant KEY-F11
0 constant KEY-F12

\ Blocking
( -- event )
: keyb-get-event
;

\ Non-blocking
( -- event|0 )
: keyb-poll-event
;

