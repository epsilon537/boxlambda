\ BoxLambda Forth
\
\ Some early definitions extending the core
\ assembly word set.

\ Register a C function so it can be called from Forth.
\ Must be defined early on because the Forth C-FFI
\ initialization code assumes it'll be there.
\ ( compile time: fun "name" -- )
\ ( run time: i*x -- j*x )
: c-fun create , does> @ call-c ;

