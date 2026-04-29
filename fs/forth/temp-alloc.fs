\ BoxLambda Forth
\
\ Temporary Memory Allocator
\

\ Save compile-to state and switch to IMEM.
compileto> variable saved-compile-to
compiletoimem

\ The temporary memory pool
create temp-space 4096 allot
temp-space variable temp-here

: x-temp-allot-failed ( -- ) ." temp allot failed" cr ;

\ Allot u bytes from temporary buffer
\ may throw x-temp-allot-failed.
\ ( u -- )
: temp-allot
  temp-here @ + ( addr+u )
  dup temp-here < averts x-temp-allot-failed ( addr+u )
  temp-here ! ( )
;

\ Get temp allocator mark. The mark can be used as an address.
: temp-mark> ( -- mark )
  temp-here @ ;

\ Revert to mark
: >temp-mark ( mark -- )
  temp-here ! ;

\ Execute xt passing in a buffer of u bytes at TOS.
\ Release buffer when xt has completed.
\ E.g. 256 [: <do-stuff-with-buf> ;] with-temp-alloc.
: with-temp-allot ( u xt -- )
  temp-mark> >r ( u xt R: mark )
  swap
  temp-allot ( xt R: mark )
  r@ swap execute ( R: mark )
  r> >temp-mark ;

\ Reset the temp allocator
: temp-allot-reset ( -- )
  temp-space temp-here ! ;

\ Restore compile-to state.
saved-compile-to @ >compileto

