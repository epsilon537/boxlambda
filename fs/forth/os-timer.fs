\ BoxLambda Forth
\ OS Timer

50000000 constant MTIMER_FREQ_HZ

( u -- ud )
: mtimer-seconds-to-clocks
  MTIMER_FREQ_HZ um*
;

( u -- ud )
: mtimer-msec-to-clocks
  mtimer-seconds-to-clocks 1000 s>d d/
;

( u -- ud )
: mtimer-usec-to-clocks
  mtimer-seconds-to-clocks 1000000 s>d d/
;

100 constant OS-TICK-MS
OS-TICK-MS mtimer-msec-to-clocks 2constant OS-TICK-CLOCKS

0 variable os-ticks

: (vanilla-os-timer-handler)
  1 os-ticks +!
;

' (vanilla-os-timer-handler) variable hook-os-timer

: (mtime-irq-handle)
  mtimecmp64 OS-TICK-CLOCKS d+ mtimecmp64!
  hook-os-timer @ execute
;

' (mtime-irq-handle) irq-timer !

( -- )
: os-timer-start
  mtime64 OS-TICK-CLOCKS d+ mtimecmp64!
  mie irq-id-timer 1<< or mie!
;

( -- )
: os-timer-stop
  -1 s>d mtimecmp64!
  mie irq-id-timer 1<< bic mie!
;

