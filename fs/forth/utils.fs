\ BoxLambda Forth
\
\ A grab bag of small utility Words that have no dependencies other than the Forth core.

\ Align an address to a power of two
: alignto ( a power -- a ) swap 1- swap 1- or 1+ ;

\ positive?
\ ( x -- flag )
: 0> ( n -- ? ) 0 > [1-foldable] ;

\ Roll takes the element u deep in the stack and moves it to the top,
\ shifting the elements above it down by one.
\ ( xu ... x0 u -- xu-1 ... x0 xu )
: roll
    ?dup if
        swap >r
        1- recurse
        r> swap
    then
;

\ Move string pointer forward by n and reduce string length by n.
\ (addr u n -- addr' u')
: /string  dup >r - swap r> chars + swap ;

\ Unsigned addition with carry.
: um+ ( u1 u2 -- u carry )
  over +            \ compute sum
  dup rot u<        \ detect unsigned overflow
;

\ Add n to d
\ ( d n -- d )
: m+ ( d n -- d' ) s>d d+ [3-foldable] ;

\ 1<<u
: bitval ( u -- u )
  1 swap lshift [inline]
;

