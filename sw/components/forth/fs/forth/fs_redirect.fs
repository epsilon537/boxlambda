const char fs_redirect_fs[] =  R"fs_redirect_fs(

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

0 variable key-fil
0 variable eof-hook
#1 buffer: key-buf

\ Set key-hook to get input from console.
\ key blocks when there is no input.
\ ( -- )
: key<console
  ['] serial-key hook-key !
  ['] serial-key hook-key? !
;

\ Read one character from key-fil file handler.
\ Call installed eof-hook when eof is reached.
\ ( -- c )
: (key<fil)
  key-fil @ key-buf 1 f_read
  0= if
    true accept-echo !
    eof-hook @ execute
    key \ retry
  else
    key-buf c@
  then
;

: (key<fil?)
  key-fil @ fs_f_eof not
;

\ Set key-hook to get input from given file handle.
\ Accept echo is disabled while input is being read from file.
\ Calls xt when eof is reached.
\ ( fil xt -- )
: key<file
  eof-hook !
  key-fil !
  false accept-echo !
  ['] (key<fil) hook-key !
  ['] (key<fil?) hook-key? !
;

)fs_redirect_fs";
