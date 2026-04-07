\ BoxLambda Forth
\
\ Structures - Based on ZeptoForth's structures, without the syntax checking.
\

\ Begin declaring a structure
: begin-structure ( "name" -- addr offset )
  <builds here 0 4 allot does> @
;

\ Finish declaring a structure
: end-structure ( addr offset -- )
  swap !
;

\ Create an arbitrary-sized field
: +field: ( offset size "name" -- offset )
  : over literal, postpone + postpone ; +
;

\ Create a byte-sized field
: cfield: ( offset "name" -- offset )
  : dup literal, postpone + postpone ; 1+
;

\ Create a halfword-sized field
: hfield: ( offset "name" -- offset )
  : haligned
    dup literal, postpone + postpone ; 2 +
;

\ Create a cell-sized field
: field: ( offset "name" -- offset )
  : aligned
    dup literal, postpone + postpone ; cell+
;

\ Create a double cell-sized field
: 2field: ( offset "name" -- offset )
  : aligned
    dup literal, postpone + postpone ; 2 cells +
;

