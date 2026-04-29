\ BoxLambda Forth
\
\ Exceptions. The definitions below are taken from ZeptoForth
\ See: https://github.com/tabemann/zeptoforth/blob/master/src/common/forth/basic.fs
\

\ Assert that a value is true, otherwise raise a specified exception
: averts ( f "name" -- )
  ' ( f xt )
  state @ if
    postpone 0=
    postpone if
    rot literal,
    postpone ?raise
    postpone then
  else
    swap 0= if
      ?raise
    else
      drop
    then
  then
  [immediate]
;

\ Assert that a value is false, otherwise raise a specified exception
: triggers ( f "name" -- )
  '
  state @ if
    postpone 0<>
    postpone if
    rot literal,
    postpone ?raise
    postpone then
  else
    swap 0<> if
      ?raise
    else
      drop
    then
  then
  [immediate]
;

\ Check whether an exception, typically returned by `try`, matches a specified
\ exception and if it does, replace it with zero, marking no exception
\ otherwise passing the specified argument through.
: suppress ( exc|0 "name" -- exc|0 )
  '
  state @ if
    postpone dup
    literal,
    postpone =
    postpone if
    postpone drop
    0 literal,
    postpone then
  else
    swap dup rot = if
      drop 0
    then
  then
  [immediate]
;

: x-assert ." Assert failed!" cr ;

\ Raise x-assert exception if f is false.
: ?assert ( f -- )
  averts x-assert
;

