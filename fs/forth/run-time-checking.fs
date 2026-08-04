\ Assert version indicating which word failed.

\ Set to zero to continue execution after a failing assert.
\ E.g. for negative testing purposes.
1 variable quit-on-xassert

\ Set to 0 to disable xassert checking.
1 variable xassert-enable

\ Put an assert statement between xassert{ ... }xassert
\ E.g xassert{ this-must-be-true }xassert
\ Any number of statements can be put between the { }, but they
\ must end with a flag.
: xassert{
  xassert-enable @ 0= if 
    begin
      nexttoken ( addr len)
      s" )xassert" compare
    until
  then
  [immediate] ;

: }xassert
  [:
    0= if 
      quit-on-xassert @ if
        r@ dup ." Assert failed at $" hex. cr
        traceinside. cr
        quit
      else
        ." Assert failed." cr
        r@ inside-code>link dup link>wid wordlist-name@ ctype ." :: " link>name ctype cr
      then
    then
  ;] compile-or-execute
  [immediate] ;

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

0 variable stack-checking-enable

128 stack-create (stack-check-stack)

: stack-check-out 
  (stack-check-stack) stack-pop
  ?dup if
    dup #16 rshift $ff and ( stackv params-out ) 
    over 8 rshift $ff and ( stackv params-out depth-in )
    rot #24 rshift $ff and ( params-out depth-in params-in ) 
    - + ( expected )
    depth 1- ( expected actual )
    2dup <> if ( expected actual )
      r@ dup ." Stack signature mismatch at $" hex. cr ( expected actual ra )
      traceinside. cr ( expected actual )
      ." Actual depth: " . cr
      ." Expected depth: " . cr
      .s cr
      quit
    else
      2drop
    then
  then
;

( out in -- )
: stack-check-in
  dup 3 + depth > if ( out in )
      r@ dup ." Stack underflow at $" hex. cr ( out in ra )
      traceinside. cr ( out in  )
      ." Actual depth: " depth 2- cr ( out in )
      ." Required depth: " dup . ( out in )
      2drop
      .s cr
      quit
  then
  \ Replace 0 entry on top of stack by an actual entry containing
  \ #in #out and depth-in
  (stack-check-stack) stack-pop drop ( out in )
  #24 lshift ( out inshifted )
  swap #16 lshift ( inshifted outshifted ) 
  or ( inoutshifted )
  depth 1- 8 lshift ( inoutshifted depth-in-shifted )
  or ( inoutdepthshifted )
  1 or ( inoutdepthshifted|1 )  
  (stack-check-stack) stack-push ( )
;

\ Invoke as follows (example):
\ ( n1 n2 -- n3 )
\ : foo
\   [ 3 1 stack-checker ]
\   ...
\ ;
\ i.e. in execution mode create a stack-checker instance and specify the number of input and
\ output params. If after the Word's execution the stack doesn't have the expected depth,
\ a failure will be reported and execution stops.
( #in #out -- )
: stack-checker stack-checking-enable @ if literal, literal, ['] stack-check-in call, else 2drop then ;

: (stack-check-in-prologue)
  \ Push a 0 entry onto the stack. It might get replaced by an actual entry by stack-check-in.
  0 (stack-check-stack) stack-push 
;

\ Redefining these to hook into Word entry and exit points...
: [: postpone [: stack-checking-enable @ if postpone (stack-check-in-prologue) then [immediate] ;

: ;] stack-checking-enable @ if postpone stack-check-out then postpone ;] [immediate] ;

: : : stack-checking-enable @ if ] postpone push_ra postpone (stack-check-in-prologue) then [immediate] ;

: ; stack-checking-enable @ if postpone stack-check-out then postpone ; [immediate] ;

: exit stack-checking-enable @ if postpone stack-check-out then postpone exit [immediate] ;


