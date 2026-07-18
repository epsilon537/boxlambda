\ Assert version indicating which word failed.

: ?assert ( f -- )
  dup 0= if 
    r@ dup ." Assert failed at $" hex. cr
    traceinside. cr then
  quit
;

