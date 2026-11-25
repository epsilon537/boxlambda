\ BoxLambda Forth
\ USB HID dispatcher

begin-module keyboard

  100 constant KEYB-DETECT-PERIOD

  \ keymods bitmask values
  $1 constant KEYMOD-LCTRL
  $2 constant KEYMOD-LSHIFT
  $4 constant KEYMOD-LALT
  $8 constant KEYMOD-LWIN
  $10 constant KEYMOD-RCTRL
  $20 constant KEYMOD-RSHIFT
  $40 constant KEYMOD-RALT
  $80 constant KEYMOD-RWIN

  \ This variable is true if a USB keyboard is connected.
  false variable keyb-present

  50 variable keyb-repeat-start-ticks
  4 variable keyb-repeat-period-ticks

  \ The scancode of the caps-lock key ( 0 for none )
  SCANCODE-CAPSLOCK variable keyb-caps-lock

  \ The scancode of the num-lock key ( 0 for none )
  SCANCODE-NUMLOCK variable keyb-num-lock

  begin-structure key-event-struct-raw
    cfield: .scancode
    cfield: .modifiers
    cfield: .upndown
    cfield: .repeat
  end-structure

  begin-structure key-event-struct
    cfield: .scancode
    cfield: .modifiers
    cfield: .upndown
    cfield: .repeat
    cfield: .key
  end-structure

  : x-keymap-not-set ." Keymap not set." cr ;
  : x-keymap-no-end  ." Keymap end sentinel not found." cr ;

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

    \ Read keymods register.
    ( -- keymods )
    : read-keymods USB_HID_0_KEY_MODS_ADDR @ ;

    \ Read keys-pressed register.
    ( -- keypress )
    : read-keypress
      USB_HID_0_KEYS_ADDR @ ( keys-pressed )
    ;

    ( -- )
    : update-keymods
      new-keymods @ prev-keymods !
      read-keymods new-keymods !
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

    create keyb-poll-latest-event key-event-struct allot

    : keyb-led! ( mask "name" -- )
      create
        \ compile-time: store the parameter in the dictionary body
        ,
      does> ( f pfa )
        @ ( f mask )
        swap if ( mask )
          USB_HID_0_LEDS_ADDR @ or USB_HID_0_LEDS_ADDR ! ( )
        else
          not USB_HID_0_LEDS_ADDR @ and USB_HID_0_LEDS_ADDR ! ( )
        then
    ;

    : keyb-led@ ( mask "name" -- )
      create
        \ compile-time: store the parameter in the dictionary body
        ,
      does> ( pfa )
        @ ( mask )
        USB_HID_0_LEDS_ADDR @ and 0<>
    ;

    1 keyb-led! led-num-lock!
    2 keyb-led! led-caps-lock!
    4 keyb-led! led-scroll-lock!
    8 keyb-led! led-compose!

    1 keyb-led@ led-num-lock@
    2 keyb-led@ led-caps-lock@
    4 keyb-led@ led-scroll-lock@
    8 keyb-led@ led-compose@

    0 variable keymap
    0 variable keymap-#keys
    \ Increments when a keypress has no mapping.
    0 variable key-not-mapped-counter
    \ boolean
    0 variable caps-locked
    \ boolean
    0 variable num-locked

    ( event -- f )
    : keypad-num?
      .scancode c@ ( scancode )
      dup SCANCODE-KEYPAD-1 >= ( scancode f )
      swap SCANCODE-KEYPAD-DOT <= ( f f )
      and
    ;

    \ event to keymap column mapping
    ( raw-event -- col )
    : event>keymap-col
      dup keypad-num? keyb-num-lock @ 0<> and if ( raw-event )
        drop ( )
        \ num-lockabled key mapping
        num-locked @ if 1 else 0 then ( col )
      else ( raw-event )
        \ non-num-lockable key mapping
        .modifiers c@ ( mod )
        case
          0 of
            caps-locked @ if 1 else 0 then ( col )
          endof
          dup [ KEYMOD-LSHIFT KEYMOD-RSHIFT or ] literal and ?of
            caps-locked @ if 0 else 1 then ( col )
          endof
          KEYMOD-RALT of
            2 ( col )
          endof
          3 swap ( col mod )
        endcase ( col )
      then ( col )
    ;

    ( raw-event -- )
    : update-counters-flags
      dup .upndown c@ 0= if ( raw-event )
        dup .scancode c@ case ( raw-event scancode )
          keyb-caps-lock @ of 
            caps-locked @ not dup caps-locked !
            led-caps-lock!
          endof
          keyb-num-lock @ of
            num-locked @ not dup num-locked !
            led-num-lock!
          endof
          over .key c@ 0= if ( raw-event scancode )
            1 key-not-mapped-counter +! ( raw-event scancode )
          then
        endcase ( raw-event )
      then ( raw-event )
      drop ( )
    ;

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
      \ set .key to 0 for the time being
      0 keyb-internals :: keyb-poll-latest-event .key c! ( )
      keyb-internals :: keyb-poll-latest-event ( event )
    else
      0
    then ( event|0 )
  ;

  \ Blocking
  ( -- raw-event )
  : keyb-get-raw-event
    begin
      keyb-poll-raw-event ( event|0)
    until ( )
    keyb-internals :: keyb-poll-latest-event ( event )
  ;


  \ Set the keymap table
  \ May throw x-keymap-no-end.
  ( keymap -- )
  : keyb-keymap!
    #256 0 do ( keymap )
      dup i 2 lshift + @ ( keymap keymap-row )
      -1 = if ( keymap )
        i keyb-internals :: keymap-#keys ! ( keymap )
        keyb-internals :: keymap ! ( )
        unloop exit ( )
      then ( keymap )
    loop
    ['] x-keymap-no-end ?raise
  ;

  \ Non-blocking
  \ May throw keymap-not-set
  ( -- event|0 )
  : keyb-poll-event
    keyb-poll-raw-event dup if ( raw-event )
      dup keyb-internals :: event>keymap-col ( raw-event col )
      over .scancode c@ ( raw-event col scancode )
      dup keyb-internals :: keymap-#keys >= if ( raw-event col scancode )
        2drop 0 ( raw-event key|0 )
      else
        2 lshift keyb-internals :: keymap @ dup averts x-keymap-not-set ( raw-event col keymap )
        + + c@ ( raw-event key|0 )
      then
      over .key c! ( event )
      dup keyb-internals :: update-counters-flags ( event )
    then
  ;

  \ Blocking
  \ May throw keymap-not-set
  ( -- event )
  : keyb-get-event
    begin
      keyb-poll-event ( event|0)
    until ( )
    keyb-internals :: keyb-poll-latest-event ( event )
  ;

  keyb-internals continue-module

    : test
      begin
        keyb-get-event ( event )
        dup .upndown c@ 0= if
          dup .scancode c@ SCANCODE-ESCAPE = if ( event )
            drop exit ( )
          then ( event )
          .key c@ emit
        then ( event )
        drop ( )
      again
    ;
  end-module
end-module

