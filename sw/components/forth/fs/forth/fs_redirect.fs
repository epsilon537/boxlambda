  0 variable emit-fil

\ ( c -- )
: (emit>fil)
  emit-fil @ swap f_putc
;

\ ( c -- )
: (tee>fil)
  dup emit-fil @ swap f_putc
  serial-emit
;

\ Set emit-hook and emit-hook? to emit-to-open-file handlers.
\ ( fil -- )
: emit>file
  emit-fil ! ( )
  ['] (emit>fil) hook-emit !
  ['] true hook-emit? !
;

\ Set emit-hook and emit-hook? to console handlers.
\ ( -- )
: emit>console
  ['] serial-emit hook-emit !
  ['] serial-emit? hook-emit? !
;

\ Set emit-hook and emit-hook? to drop all output.
\ ( -- )
: emit>null
  ['] drop hook-emit !
  ['] true hook-emit? !
;

\ Set emit-hook and emit-hook? to emit-to-open-file-and-console handlers.
\ Install emit hook forwarding to open file with given description,
\ then invoke previous emit handler.
\ ( fil -- )
: tee>file
  emit-fil ! ( )
  ['] (tee>fil) hook-emit !
  ['] serial-emit? hook-emit? !
;

\ Remove tee>file emit hook. Restore previous emit handler.
\ ( -- )
: tee-end
  emit>console
;

