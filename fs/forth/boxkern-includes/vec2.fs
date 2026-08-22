\ BoxLambda Forth
\ 2D vector

compileto-save
compiletoimem

( x y -- vec )
: vec2 16 lshift swap $ffff and or [inline] ;

( v1 v2 -- vsum )
: vec2+
  2dup + $ffff and ( v1 v2 xsum )
  -rot $ffff0000 and ( xsum v1 v2 )
  + $ffff0000 and ( xsum ysum )
  or
;

( v1 v2 -- vdiff )
: vec2-
  2dup - $ffff and ( v1 v2 xdiff )
  -rot $ffff0000 and 
  - $ffff0000 and ( xdiff ydiff )
  or
;

( v -- x )
: vec2.x $ffff and [inline] ;

( v -- y )
: vec2.y 16 rshift [inline] ;

( v -- x y )
: vec2.xy dup $ffff and swap 16 rshift [inline] ;

( v1 v2 -- v1.v2 )
: vec2dot
  2dup vec2.x swap vec2.x * ( v1 v2 x1.x2 )
  -rot vec2.y swap vec2.y * + ( x1.x2 + y1.y2 )
;

\ Regular * is faster, but this version avoid overflow rollover in the other dimension.
( v n -- v )
: vec2* 
  swap
  2dup vec2.x * (  n v x )
  -rot vec2.y * ( x y )
  vec2
;

( v -- )
: .vec2 vec2.xy ." ( " swap . ." , " . ." )" ;

compileto-restore

