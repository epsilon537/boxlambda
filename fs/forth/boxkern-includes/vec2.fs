\ BoxLambda Forth
\ 2D vector Words

compileto-save
compiletoimem

\ Create a 2D vector value out of x and y coordinates. Only the lower 16 bits of the x and y values are used.
( x y -- vec )
: vec2 16 lshift swap $ffff and or [inline] ;

\ Add 2 2D vectors.
( v1 v2 -- vsum )
: vec2+
  2dup + $ffff and ( v1 v2 xsum )
  -rot $ffff0000 and ( xsum v1 v2 )
  + $ffff0000 and ( xsum ysum )
  or
;

\ Substract 2 2D vectors.
( v1 v2 -- vdiff )
: vec2-
  2dup - $ffff and ( v1 v2 xdiff )
  -rot $ffff0000 and 
  - $ffff0000 and ( xdiff ydiff )
  or
;

\ Extract the x components of a 2D vector.
( v -- x )
: vec2.x $ffff and [inline] ;

\ Extract the y component of a 2D vector.
( v -- y )
: vec2.y 16 rshift [inline] ;

\ Extract the x and y components of a 2D vector.
( v -- x y )
: vec2.xy dup $ffff and swap 16 rshift [inline] ;

\ Compute the dot product of 2 2D vectors.
( v1 v2 -- v1.v2 )
: vec2dot
  2dup vec2.x swap vec2.x * ( v1 v2 x1.x2 )
  -rot vec2.y swap vec2.y * + ( x1.x2 + y1.y2 )
;

\ Scale a 2D vector.
\ Regular * is faster, but this version avoid overflow rollover in the other dimension.
( v n -- v )
: vec2* 
  swap
  2dup vec2.x * (  n v x )
  -rot vec2.y * ( x y )
  vec2
;

\ Print a 2D vector.
( v -- )
: .vec2 vec2.xy ." ( " swap . ." , " . ." )" ;

compileto-restore

