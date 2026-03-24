\ BoxLambda Forth
\
\ Some early definitions extending the core
\ assembly word set.

\ Setting the MTIMER Comparator
: set-raw-time-cmp ( u -- ) s>d mtime64 d+ mtimecmp64! ;

\ IRQ ID constants
16 constant irq-id-fast-0
7 constant irq-id-timer
13 irq-id-fast-0 + constant irq-id-vera
12 irq-id-fast-0 + constant irq-id-vs00
08 irq-id-fast-0 + constant irq-id-dfx
10 irq-id-fast-0 + constant irq-id-sdpsi
08 irq-id-fast-0 + constant irq-id-usb-hid-1
07 irq-id-fast-0 + constant irq-id-usb-hid-0
07 irq-id-fast-0 + constant irq-id-i2c
05 irq-id-fast-0 + constant irq-id-uart

\ max and min int values
0 invert                    constant max-uint
0 invert 1 rshift           constant max-int
0 invert 1 rshift invert    constant min-int

\ Register a C function so it can be called from Forth.
\ Define: ( fun "name" -- )
\ Execute: ( i*x -- j*x )
: c-fun create , does> @ call-c ;

\ Interactive string printing.
: ." ( -- )
  state @ if
    postpone ."
  else
    [char] " parse type cr
  then
[immediate] ;

\ Align an address to a power of two
: alignto ( a power -- a ) swap 1- swap 1- or 1+ ;

\ 1 cell size
4 constant cell

\ Add the size of n cells to x)
\ ( x n -- x+n*cell )
: cells+ cell * + ;

\
\ array
\
\ Cell array
: array ( n -- ) ( i -- addr)
     create cells allot
     does> swap cells+ ;

\ Byte array
: carray ( n -- ) ( i -- addr)
     create cells allot
     does> swap + ;

\ positive?
\ ( x -- flag )
: 0> ( n -- ? ) 0 > [1-foldable] ;

: within ( n1|u1 n2|u2 n3|u3 -- flag ) over - >r - r> u< [3-foldable] ;

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

\ chars and char+
: chars ( u -- u ) [0-foldable] ;
: char+ ( u -- u+1 ) 1+ [1-foldable] ;

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
: bit ( u -- u )
  1 swap lshift [inline]
;

