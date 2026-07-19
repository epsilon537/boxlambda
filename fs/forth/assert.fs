\ Assert version indicating which word failed.

: ?assert ( f -- )
  0= if 
    r@ dup ." Assert failed at $" hex. cr
    traceinside. cr
    quit
  then
;

