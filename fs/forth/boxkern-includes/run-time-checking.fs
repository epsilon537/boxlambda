\ BoxLambda Forth
\ Run-time Checking tools.

\ Set to 1/0 to enable/disable xassert checking.
0 variable xassert-enable

\ Set to 1/0 to enable/disable stack checking.
0 variable stack-checking-enable

\ Set to 1/0 to enable/disable run-time type checking on structs.
0 variable rttc-struct

\ -- Assert version that can be compiled out entirely and indicates which Word failed.

\ Put an assert statement between xassert{ ... }xassert.
\ E.g xassert{ this-must-be-true }xassert
\ Any number of statements can be put between the { }, but they must end with a flag.
( compile: -- | run-time -- )
: xassert{
  xassert-enable @ 0= if 
    begin
      nexttoken ( addr len)
      s" }xassert" compare
    until
  then
  [immediate] [compileonly] ;

( compile: -- | run-time: f -- )
: }xassert
  postpone 0=
  postpone if
  ['] x-assert \ Raise an x-assert exception if the assert fails.
  literal,
  postpone ?raise
  postpone then
  [immediate] [compileonly] ;

\ -- Run-time type checking on structs.

0 variable (init-type)

: x-typecheck-failed ." Typecheck failed." cr ;

\ Defining Word: Create a typechecker instance for structs
( init-type typecheck run-time: struct-inst -- )
( typecheck run-time: struct-inst -- struct-inst )
: typechecker
  create FLAG-IMMEDIATE setflags \ Make the created Word immediate.
  does> ( compile: type-id | run-time: struct-inst ) \ typechecker instance address serves as type-id
  rttc-struct @ if \ Is run-time type checking enabled?
    literal, ( compile: | run-time: struct-inst type-id )
    (init-type) @ if \ Is the typechecker being initialized, i.e. invoked as "init-type typecheck"?
      false (init-type) ! \ Reset (init-type) to false.
      postpone swap ( run-time: type-id struct-inst )
      postpone ! ( run-time: ) \ Store the type-id in the first cell of the struct-inst.
    else \ The typecheck is invoked to typecheck
      postpone over ( run-time: struct-inst type-id struct-inst )
      postpone @ ( run-time: struct-inst type-id struct-inst-type-id )
      postpone <>
      postpone if ( struct-inst ) \ if the struct-inst-type-id doesn't match the excpected type-id...
      ['] x-typecheck-failed literal, ( struct-inst exception )
      postpone ?raise
      postpone then ( struct-inst )
    then
  else  \ run-time typechecking is disabled.
    drop ( compile: | run-time: struct-inst )
    (init-type) @ if
      false (init-type) !
      postpone drop ( run-time: )
    then
  then
;

\ Set the struct instance to the given type.
( struct-inst "typechecker" -- )
: init-type
  true (init-type) ! ( struct-inst ) \ Set (init-type) to true
  ' execute ( ) \ then execute the typechecker. This initializes the typechecker.
  [immediate] [compileonly] 
;

\ Begin declaring a structure. This redefines struct.fs's begin-structure.
\ In this version, if rttc-struct is enabled, the struct contains an extra cell as
\ first "field", containing (after initialization) the struct's type identifier.
( "name" -- addr offset )
: begin-structure
  create here
  rttc-struct @ if
    4
  else
    0 
  then
  4 allot does> @
;

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

\ -- Stack Checking:

\ Stack checking can be nested, so we need a stack to keep track of things.
128 stack-create (stack-check-stack)

\ Initialize previous on-quit hook with current on-quit hook.
hook-on-quit @ variable (stack-check-prev-on-quit-hook)

\ On quit: reset the stack-check stack.
( -- )
: (stack-check-on-quit-hook)
  \ Reset stack-check-stack on quit
  (stack-check-stack) stack-reset
  (stack-check-prev-on-quit-hook) @ execute \ Proceed with the next on-quit hook in the chain.
;

\ Install the on-quit hook.
' (stack-check-on-quit-hook) hook-on-quit !

\ (stack-check-out) runs at the exit of every Word, regardless of whether stack checking
\ is enabled for that Word or not.
\ When stack checking is enabled, check if the stack depth matches expectations.
( -- )
: (stack-check-out)
  stack-checking-enable @ if
    (stack-check-stack) stack-pop ( stackv )
    ?dup if \ If non-zero, decode the stackv value.
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
  then
;

\ Upon entering a stack-checked Word, do some preliminary checks on stack depth,
\ then prepare and push a stackv value to be checked against upon check-out (see above).
( out in -- )
: (stack-check-in)
  stack-checking-enable @ if
    dup 3 + depth > if ( out in )
        r@ dup ." Stack underflow at $" hex. cr ( out in ra )
        traceinside. cr ( out in  )
        ." Actual depth: " depth 2- . cr ( out in )
        ." Required depth: " dup . ( out in )
        2drop
        .s cr
        quit
    then
    \ A 0 value has already been pushed on the stack-check-stack by the (stack-check-in-prologue).
    \ Replace this 0 entry with an actual stackv entry containing #in #out and depth-in.
    (stack-check-stack) stack-pop drop ( out in )
    #24 lshift ( out inshifted )
    swap #16 lshift ( inshifted outshifted )
    or ( inoutshifted )
    depth 1- 8 lshift ( inoutshifted depth-in-shifted )
    or ( inoutdepthshifted )
    1 or ( inoutdepthshifted|1 )
    (stack-check-stack) stack-push ( )
  else
    2drop
  then
;

\ Invoke as follows (example):
\ ( n1 n2 -- n3 )
\ : foo
\   [ 2 1 stack-checker ]
\   ...
\ ;
\ i.e. in execution mode create a stack-checker instance and specify the number of input and
\ output params. If after the stack checked Word's execution the stack doesn't have the expected depth,
\ a failure will be reported and execution stops.
( #in #out -- )
: stack-checker
  stack-checking-enable @ if
    \ Compile a call to (stack-check-in) with expected #in and #out params on the stack.
    literal, literal, ['] (stack-check-in) call,
  else 2drop then 
;

\ (stack-check-in-prologue) runs a the entry to every Word, regardless of weather we're doing stack checking
\ on that Word or not.
( -- )
: (stack-check-in-prologue)
  stack-checking-enable @ if
    \ Push a 0 entry onto the stack. It might get replaced by an actual entry by stack-check-in.
    0 (stack-check-stack) stack-push 
  then
;

\ Redefining these to hook stack checking logic into every subsequent Word entry and exit points...
\ Note that it all compiles away if stack-checking-enable is set to false.
\ Note also the following limitation: There's no hook for Word exits through a raised exception,
\ i.e. when running tests with stack-checking enabled, raised exceptions, when caught by try may
\ leave the stack-checking stack unbalanced.

: [: postpone [: stack-checking-enable @ if postpone (stack-check-in-prologue) then [immediate] ;

: ;] stack-checking-enable @ if postpone (stack-check-out) then postpone ;] [immediate] ;

: : : stack-checking-enable @ if ] postpone push_ra postpone (stack-check-in-prologue) then [immediate] ;

: does> postpone does> stack-checking-enable @ if postpone (stack-check-in-prologue) then [immediate] ;

: ; stack-checking-enable @ if postpone (stack-check-out) then postpone ; [immediate] ;

: exit stack-checking-enable @ if postpone (stack-check-out) then postpone exit [immediate] ;

