\ BoxLambda Forth
\
\ Structures with Run-Time Type Checking support.
\ This didn't fit in the original struct.fs because [ifdef]
\ isn't available yet at that point.
\

\ Begin declaring a structure
: begin-structure ( "name" -- addr offset )
  create here 
[ifdef] RTTC
  4
[else]
  0 
[then]
  4 allot does> @
;

