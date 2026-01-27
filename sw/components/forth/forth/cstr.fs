const char cstr_fs[] =  R"cstr_fs(
\ C FFI for strings

255 constant CSTRLENMAX

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

\ ( adr -- adr len ) Convert C string to Forth string.
: s0>s dup s0len ;

\ 0-terminate Forth string, assumes there's space for the 0-terminator.
\ ( c-addr len -- )
: +0c! + 0 swap c! ;

\ Compiles a 0-terminated string and gives back its address when executed.
: s0"
  ['] (s") call, \ Compile a call to (s")
  here \ Remember where the string will be inserted. ( insert-addr )
  34 parse \ Parse up to closing ". ( savedhere a-addr len )j
  1+ \ Incr. length to make space for 0-term
  dup CSTRLENMAX u> if
    ." String too long" cr
  then
  tuck \ saved it for later. ( insert-addr len+1 a-addr len+1 )
  string, \ Insert the counted string. ( insert-addr len+1 )
  +0c! \ Write 0 at the end. ()
  \ (s") returns to this point, the 1st instruction after the inserted string.
  ['] drop inline, \ drop length return by (s"), just return addr.
  [immediate]
;

\ Copy len bytes from c-addr to buf of at least 256 bytes and add 0 terminator
\ ( buf256 c-addr len -- )
: cstr
  dup CSTRLENMAX u> if
    ." String too long" cr
  then
  rot swap  \ ( c-addr buf len )
  2dup +0c! \ 0-terminate. ( c-addr buf len )
  move      \ ( )
;

\ ( buf256 "text" -- ) copy Forth string into buffer of at least 256 bytes and 0-terminate.
: cstr"
  34 parse \ ( buf256 c-addr len )
  cstr   \ ( )
;

)cstr_fs";
