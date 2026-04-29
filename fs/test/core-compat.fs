\ BoxLambda Forth:
\ The definitions below are referenced by the original Mecrisp testsuite.fs.
\ They are useful Forth standard core and core extension words, so I put them
\ in their own module. They are currently not considered part of the BoxLambda OS
\ Word set, however.

\ ------------------------------------------------------------------------
\  These CORE definitions are not part of Mecrisp-Quintus for default:
\ ------------------------------------------------------------------------

256 buffer: BUF0

: word ( c -- c-addr )
    begin
        source >r >in @ + c@ over =
        r> >in @ xor and
    while
        1 >in +!
    repeat

    parse
    dup BUF0 c!
    BUF0 1+ swap move
    BUF0
;


: erase ( addr u -- ) 0 fill ;


: sgn ( u1 n1 -- n2 ) \ n2 is u1 with the sign of n1
    0< if negate then
[2-foldable] ;

\ Divide d1 by n1, giving the symmetric quotient n3 and the remainder
\ n2.
: sm/rem ( d1 n1 -- n2 n3 )
    2dup xor >r     \ combined sign, for quotient
    over >r         \ sign of dividend, for remainder
    abs >r dabs r>
    um/mod          ( remainder quotient )
    swap r> sgn     \ apply to remainder
    swap r> sgn     \ apply to quotient
[3-foldable] ;

\ Divide d1 by n1, giving the floored quotient n3 and the remainder n2.
\ Adapted from hForth
: fm/mod ( d1 n1 -- n2 n3 )
    dup >r 2dup xor >r
    >r dabs r@ abs
    um/mod
    r> 0< if
        swap negate swap
    then
    r> 0< if
        negate         \ negative quotient
        over if
            r@ rot - swap 1-
        then
    then
    r> drop
[3-foldable] ;

\ : */mod ( n1 n2 n3 -- n4 n5 ) >r m* r> sm/rem [3-foldable] ;
\ : */    ( n1 n2 n3 -- n4 )    */mod nip [3-foldable] ;

: 1/string 1 /string ;


: isspace? ( c -- f )
    $21 u< ;

: isnotspace? ( c -- f )
    isspace? 0= ;

: xt-skip   ( addr1 n1 xt -- addr2 n2 ) \ gforth
    \ skip all characters satisfying xt ( c -- f )
    >r
    BEGIN
        over c@ r@ execute
        over and
    WHILE
        1/string
    REPEAT
    r> drop ;


: parse-name ( "name" -- c-addr u )
    source >in @ /string
    ['] isspace? xt-skip over >r
    ['] isnotspace?
    xt-skip ( end-word restlen r: start-word )
    2dup 1 min + source drop - >in !
    drop r>
    tuck -
;

: w@unaligned ( addr -- x ) dup h@ swap 2+ h@ 16 lshift or ;
: >body ( addr -- addr* )
  begin dup 4 - w@unaligned $000FFFFF and $00078467 ( jalr x8, ...[x15] ) <> while 2 + repeat [1-foldable] ;
: >bdy 4 - w@unaligned .s ;

\ ------------------------------------------------------------------------
\  These CORE-EXT definitions are not part of Mecrisp-Quintus for default:
\ ------------------------------------------------------------------------

: compile, ( addr -- ) call, ;

: :noname ( -- addr ) 0 s" : (noname)" evaluate drop (latest) @ 2 cells + skipstring ;

\ ------------------------------------------------------------------------
\  These DOUBLE definitions are not part of Mecrisp-Quintus for default:
\ ------------------------------------------------------------------------

 0 CONSTANT <FALSE>
-1 CONSTANT <TRUE>

: literal ( n -- ) literal, [immediate] ;

: 2literal ( d -- )
    swap postpone literal postpone literal
; [immediate]

: d>s ( d -- n ) drop ;


