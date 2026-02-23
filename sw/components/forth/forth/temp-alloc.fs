const char temp_alloc_fs[] =  R"temp_alloc_fs(

compileto> variable saved-compile-to

compiletoimem

create temp-space 1024 allot
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

: with-temp-allot ( u xt -- )
  temp-mark> >r ( u xt R: mark )
  swap temp-allot ( xt R: mark )
  r@ swap execute ( R: mark )
  r> >temp-mark ;

\ Reset the temp allocator
: temp-allot-reset ( -- )
  temp-space temp-here ! ;

saved-compile-to @ >compileto

)temp_alloc_fs";

