\ BoxLambda Forth
\ C FFI for strings

255 constant CSTRLENMAX

: x-string-too-long ." String too long." cr ;

\ ( adr -- len ) calculate the length of a 0-terminated string.
: s0len  \ ( c-addr -- u )
  dup \ ( start-addr cur-addr )
  begin
    dup c@ \ ( start-addr cur-addr c )
  while \ ( start-addr cur-addr )
    1+ \ ( start-addr cur-addr+1 )
  repeat
  swap - \ ( len )
;

\ Convert 0-terminated string to Forth string.
\ ( adr -- adr len )
: s0>s dup s0len ;

\ 0-terminate Forth string, assume there's space for the 0-terminator.
\ ( adr len -- )
: +0c! + 0 swap c! ;

\ Compiles a 0-terminated string and gives back its address when executed.
\ May raise x-string-too-long.
\ ( "string" -- adr )
: s0"
  ['] (s") call, \ Compile a call to (s")
  here \ Remember where the string will be inserted. ( insert-addr )
  [char] " parse \ Parse up to closing ". ( savedhere parsed-addr len )
  1+ \ Incr. length to make space for 0-term ( savedhere parsed-addr len+1 )
  dup CSTRLENMAX u> triggers x-string-too-long
  tuck \ save it for later. ( insert-addr len+1 parsed-addr len+1 )
  string, \ Insert the counted string. ( insert-addr len+1 )
  +0c! \ Write 0 at the end. ()
  \ (s") returns to this point, the 1st instruction after the inserted string.
  ['] drop inline, \ drop length return by (s"), just return addr.
  [immediate] [compileonly]
;

\ Copy len bytes from addr to buf256 of at least 256 bytes and add 0-terminator
\ May raise x-string-too-long.
\ ( buf256 adr len -- )
: cstr
  dup CSTRLENMAX u> triggers x-string-too-long
  rot swap  \ ( adr buf len )
  2dup +0c! \ 0-terminate. ( adr buf len )
  move      \ ( )
;

\ Copy Forth string into buffer of at least 256 bytes and 0-terminate.
\ May raise x-string-too-long.
\ ( buf256 "text" -- )
: cstr"
  [char] " parse \ ( buf256 c-addr len )
  cstr   \ ( )
;

