\ BoxLambda Forth
\ Iterators - based on Zeptoforth iterators.

\ Iterate executing an xt over a byte array
: citer ( addr count xt -- ) ( xt: c -- )
  >r ( R: xt )
  over + ( addr end-addr R: xt )
  begin
    2dup u< ( addr end-addr R: xt )
    while
      over c@ r@ execute ( addr end-addr R: xt )
      swap 1+ swap
  repeat
  2drop rdrop
;

\ Iterate executing an xt over a halfword array
: hiter ( addr count xt -- ) ( xt: h -- )
  >r ( R: xt )
  2* over + ( addr end-addr R: xt )
  begin
    2dup u< ( addr end-addr R: xt )
    while
      over h@ r@ execute ( addr end-addr R: xt )
      swap 2+ swap
  repeat
  2drop rdrop
;

\ Iterate executing an xt over a cell array
: iter ( addr count xt -- ) ( xt: x -- )
  >r ( R: xt )
  cells over + ( addr end-addr R: xt )
  begin
    2dup u< ( addr end-addr R: xt )
    while
      over @ r@ execute ( addr end-addr R: xt )
      swap cell+ swap
  repeat
  2drop rdrop
;

\ Anonymous lists

\ Start an anonymous list of cells, e.g. l{ 4 ,  5 , 6 ... 
( -- )
: l{ 
  postpone [ ( ) \ switch to execution mode
  postpone ahead ( ahead structmatchconst ) \ Jump over the list items
  here ( ahead structmatchconst liststart ) \ Keep track of the start of the list
  [immediate] [compileonly] 
;

\ Terminate an anonymous list of cells and get its address and count.
\ E.g. ... 8 , 9 }l
( -- addr count )
: }l ( ahead structmatchconst liststart listitem )
  , ( ahead structmatchconst liststart ) \ write the last item in the list 
  here ( ahead structmatchconst liststart listend ) \ Keep track of the end of the list
  2swap postpone then ( liststart listend ) \ word execuction resumes here.
  unspan cell/ ( addr count ) \ Convert list start/end to addr/count 
  swap literal, literal, ( ) \ Write the addr and count literals
  ] \ Return to compilation-mode
;

\ Start an anonymous list of halfwords, e.g. h{ 4 h,  5 h, 6 ... 
( -- )
: h{ 
  postpone [ ( )
  postpone ahead ( ahead structmatchconst )
  here ( ahead structmatchconst liststart )
  [immediate] [compileonly] 
;

\ Terminate an anonymous list of halfwords and get its address and count.
\ E.g. ... 8 h, 9 }h
( -- addr count )
: }h ( ahead structmatchconst liststart listitem )
  h, ( ahead structmatchconst liststart ) 
  here ( ahead structmatchconst liststart listend )
  align
  2swap postpone then ( liststart listend )
  unspan 2/ ( addr count )
  swap literal, literal, ( ) 
  ]
;

\ Start an anonymous list of bytes, e.g. c{ 4 c,  5 c, 6 ... 
( -- )
: c{ 
  postpone [ ( )
  postpone ahead ( ahead structmatchconst )
  here ( ahead structmatchconst liststart )
  [immediate] [compileonly] 
;

\ Terminate an anonymous list of bytes and get its address and count.
\ E.g. ... 8 c, 9 }c
( -- addr count )
: }c ( ahead structmatchconst liststart listitem )
  c, ( ahead structmatchconst liststart ) 
  here ( ahead structmatchconst liststart listend )
  align \ Make sure the following instructions start at an aligned address.
  2swap postpone then ( liststart listend )
  unspan ( addr count )
  swap literal, literal, ( ) 
  ]
;

