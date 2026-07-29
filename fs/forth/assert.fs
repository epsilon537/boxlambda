\ Assert version indicating which word failed.

\ Set to zero to continue execution after a failing assert.
\ E.g. for negative testing purposes.
1 variable quit-on-xassert

\ Set to 0 to disable xassert checking.
1 variable xassert-enable

\ Pass in an xt that returns a flag. If the flag is false, report assert failure and location of failure.
: xassert ( xt -- )
  xassert-enable @ if
    [:
      execute 0= if 
        quit-on-xassert @ if
          r@ dup ." Assert failed at $" hex. cr
          traceinside. cr
          quit
        else
          ." Assert failed." cr
          r@ inside-code>link dup link>wid wordlist-name@ ctype ." :: " link>name ctype cr
        then
      then
    ;] 
  else
    [: drop ;]
  then
  compile-or-execute [immediate]
;

0 variable (init-type)

\ Create a typechecker instance for structs
\ ( "name" -- ) 
\ Execute: ( struct-inst -- )
: typechecker
  create FLAG-IMMEDIATE setflags \ Make the created Word immediate.
  does> 
[ifdef] RTTC
  literal, 
  (init-type) @ if
    false (init-type) !
    [: swap ! ;]
  else
    [: over @ <> if r@ dup ." Typecheck failed at $" hex. cr traceinside. cr quit then ;]
  then
  compile-or-execute
[else]
  drop
  (init-type) @ if 
    false (init-type) !
    [: drop ;] compile-or-execute
  then
[then]
;

\ Set the struct instance to the given type.
( struct-inst "typechecker" -- )
: init-type true (init-type) ! ' execute [immediate] [compileonly] ;

\ Usage example:
\
\   begin-structure tilemap-struct
\     field:  .base
\     hfield: .width
\     hfield: .height
\     cfield: .type
\   end-structure
\
\   typechecker tm-typecheck
\
\   \ Initialize the tilemap object.
\   \ ( tilemap -- )
\   : init 
\     dup tilemap-struct 0 fill 
\     init-type tm-typecheck
\   ;
\
\   \ Retrieve map width from the tilemap object.
\   \ ( tilemap -- width )
\   : width@ 
\     tm-typecheck \ print message with wordname/location if typecheck fails.
\     .width h@ ;

