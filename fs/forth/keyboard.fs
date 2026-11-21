\ BoxLambda Forth
\ USB HID dispatcher

begin-module keyboard

  100 constant KEYB-DETECT-PERIOD

  \ keymods bitmask values
  $1 constant KEYMOD-CTRL
  $2 constant KEYMOD-SHIFT
  $4 constant KEYMOD-ALT
  $8 constant KEYMOD-WIN

  \ This variable is true if a USB keyboard is connected.
  false variable keyb-present

  50 variable keyb-repeat-start-ticks
  4 variable keyb-repeat-period-ticks

  begin-structure key-event-struct-raw
    cfield: .scancode
    cfield: .modifiers
    cfield: .upndown
    cfield: .repeat
  end-structure

  (  key-event-raw -- )
  : keyb-event.
    >r
    r@ .modifiers c@ r@ .scancode c@
    s" Raw key event: scancode: $%x modifiers: $%x " printf
    r@ .upndown c@ if ." up " else ." down " then
    r@ .repeat c@ if ." repeat" then
    cr
    rdrop
  ;

  \ Enable/disable software test mode
  ( f -- )
  : keyb-sw-test-mode USB_HID_0_STATUS_BLOCK_REPORTS! ;

  \ Returns software test mode
  ( -- f )
  : keyb-sw-test-mode? USB_HID_0_STATUS_BLOCK_REPORTS@ 0<> ;

  begin-module keyb-internals

    hook-os-timer @ variable prev-hook-timer
    0 variable detect-ticks
    0 variable report-count

    0 variable repeat-ticks
    -1 variable repeat-ticks-cmp

    ( -- )
    : keyb-detect
      detect-ticks @ 1+ ( ticks )
      dup KEYB-DETECT-PERIOD >= if ( ticks )
        drop 0 ( ticks)
        report-count @ 0> ( ticks f )
        dup keyb-present ! ( ticks f )
        dup led-keyboard-detect! 0= ( ticks f )
        keyb-sw-test-mode? not and ( ticks f )
        \ Reset the core if keyboard is no longer present to allow re-detect/re-enumeration.
        if
          RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL!
        then ( ticks )
        0 report-count ! ( ticks )
      then
      detect-ticks ! ( )
    ;

    0 variable prev-keypress \ The previous keys-pressed register value.
    0 variable new-keypress  \ The current keys-pressed register value.
    0 variable prev-keymods \ The previous USB_HID_0_KEY_MODS register value. Left/right folded.
    0 variable new-keymods \ The current USB_HID_0_KEY_MODS register value. Left/right folded.

    32 queue-create keyb-q

    \ Read keymods register and fold left/right.
    ( -- keymods )
    : read-keymods-fold
      USB_HID_0_KEY_MODS_ADDR @ dup 4 rshift or $f and
    ;

    \ Read keys-pressed register.
    ( -- keypress )
    : read-keypress
      USB_HID_0_KEYS_ADDR @ ( keys-pressed )
    ;

    ( -- )
    : update-keymods
      new-keymods @ prev-keymods !
      read-keymods-fold new-keymods !
    ;

    ( -- )
    : update-keypress
      new-keypress @ prev-keypress !
      keyb-q queue-#free 8 < if
        \ If the keyboard queue is nearly full, don't process new keypresses.
        \ Act as if all keys are released. Nearly full to allow the key release events
        \ to go through.
        0 new-keypress !
      else
        read-keypress new-keypress !
      then
    ;

  end-module

  create key-event-raw key-event-struct-raw allot

  ( -- )
  : keyb-q-hook
    \ Pick up all 4 bytes at once.
    key-event-raw .scancode @ keyb-internals :: keyb-q queue-push
  ;

  ' keyb-q-hook variable key-event-raw-hook

  keyb-internals continue-module

    ( scancode upndown repeat -- )
    : report-key-event
      -rot ( repeat scancode upndown )
      swap key-event-raw .scancode c! ( repeat upndown )
      dup if prev-keymods else new-keymods then @ ( repeat upndown keymods )
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
    : process-keychanges
      >r ( check against R: upndown )
      >r ( check R: upndown against )
      begin
        dup while ( check R: upndown against )
          dup $ff and ( check key R: upndown against )
          dup r@ find-byte-in-word 0= if ( check key R: upndown against )
            \ mismatch -> key-event.
            dup 1 rpick false ( key upndown repeat )
            report-key-event ( check key R: upndown against )
          then
          drop 8 rshift ( check' R: upndown against )
      repeat ( check R: upndown against )
      drop ( R: upndown against )
      2rdrop ( )
    ;

    ( -- )
    : process-keyups \ Are any of the old keys no longer present in the new key-presses?
      prev-keypress @ ( prev-keys )
      new-keypress @ ( prev-keys new-keys )
      true process-keychanges ( )
    ;

    ( -- )
    : process-keydns \ Are any of the new keys not yet present in the old key-presses?
      new-keypress @ ( new-keys )
      prev-keypress @ ( new-keys prev-keys )
      false process-keychanges ( )
    ;

    ( v -- )
    : repeat-timer-start
      repeat-ticks-cmp !
      0 repeat-ticks ! \ Restart the timer
    ;

    ( -- )
    : repeat-timer-stop -1 repeat-ticks-cmp ! ;

    ( -- )
    : process-keyreport
      update-keymods
      update-keypress
      new-keypress @ prev-keypress @ <> if \ any key-press changes?
        process-keyups
        process-keydns
        new-keypress @ if \ any keys pressed? -> (re)start the repeat timer.
          keyb-repeat-start-ticks @ repeat-timer-start
        else \ no keys pressed currently -> cancel repeat timer
          repeat-timer-stop
        then
      then
    ;

    : usb-hid-0-irq-handle
      USB_HID_0_ISR_USB_REPORT@ if
        \ Acknowledge the USB report interrupt in the USB core.
        1 USB_HID_0_ISR_USB_REPORT!
        USB_HID_0_STATUS_USB_TYP@ USB_HID_0_STATUS_USB_TYP_KEYB = if
          1 report-count +!
          process-keyreport
        then
      then
    ;

    ' usb-hid-0-irq-handle irq-usb_hid_0 !

    0 variable no-repeat-keys

  end-module

  ( k0 k1 k2 k3 -- )
  \ Specify 4 scancodes that won't repeat on long-press. 0 is dummy value.
  : keyb-no-repeat-keys!
    0 ( k0 k1 k2 k3 acc )
    4 0 do ( k0..kn acc )
      swap ?dup if ( k0..kn-1 acc kn ) \ non-zero keys only.
        swap 8 lshift ( k0..kn acc' )
        or ( k0..kn-1 acc )
      then
    loop
    keyb-internals :: no-repeat-keys !
  ;

  ( -- k0 k1 k2 k3 )
  : keyb-no-repeat-keys@
    4 0 do
      keyb-internals :: no-repeat-keys @ ( k0..kn-1 acc )
      i 8 * rshift $ff and ( k0 .. kn )
    loop
  ;

  keyb-internals continue-module

    ( -- )
    : report-keypress-repeat
      new-keypress @
      begin ( keypresses )
        dup while ( keypresses )
          dup $ff and ( keypresses key )
          dup no-repeat-keys @ find-byte-in-word not ( keypresses key f ) \ only repeat keys
          keyb-q queue-#free 8 >= and ( keypresses key f ) \ and only if queue not full
          if ( keypresses key )
            dup false true report-key-event ( keypresses key )
          then
          drop
          8 rshift ( keypresses )
      repeat
      drop ( )
    ;

    : keyb-repeat
      repeat-ticks @ 1+ ( ticks )
      dup repeat-ticks ! ( ticks )
      repeat-ticks-cmp @ u>= if ( )
        keyb-repeat-period-ticks @ repeat-timer-start
        report-keypress-repeat
      then
    ;

    ( -- )
    : keyb-os-tick
      keyb-detect
      keyb-repeat
      prev-hook-timer @ execute
    ;

    create keyb-poll-latest-event key-event-struct-raw allot

  end-module

  ( -- )
  : keyb-start
    \ Reset the core.
    RESET_CONTROL_CTRL_USB_RESET RESET_CONTROL_CTRL!
    \ Clear the USB report interrupt in the USB core.
    1 USB_HID_0_ISR_USB_REPORT!
    1 USB_HID_0_IEN_USB_REPORT!
    mie irq-id-usb-hid-0 1<< or mie!
    keyb-internals ::['] keyb-os-tick hook-os-timer !
  ;

  ( -- )
  : keyb-stop
    0 USB_HID_0_IEN_USB_REPORT!
    mie irq-id-usb-hid-0 1<< bic mie!
    keyb-internals ::['] prev-hook-timer hook-os-timer !
  ;

  \ Reset the keyboard queue.
  ( -- )
  : keyb-reset
    keyb-internals :: keyb-q queue-reset
  ;

  ( -- f )
  : keyb-overflow?
    keyb-internals :: keyb-q queue-#free 8 <
  ;

  ( -- n )
  : keyb-#events-pending
    keyb-internals :: keyb-q queue-depth
  ;

  \ Non-blocking
  ( -- event|0 )
  : keyb-poll-raw-event
    keyb-internals :: keyb-q queue-depth 0> if ( )
      keyb-internals :: keyb-q queue-pop ( event-val )
      \ store all 4 bytes at once.
      keyb-internals :: keyb-poll-latest-event .scancode ! ( )
      keyb-internals :: keyb-poll-latest-event ( event )
    else
      0
    then ( event|0 )
  ;

  \ Blocking
  ( -- event )
  : keyb-get-raw-event
    begin
      keyb-poll-raw-event ( event|0)
    until ( )
    keyb-internals :: keyb-poll-latest-event ( event )
  ;

end-module

