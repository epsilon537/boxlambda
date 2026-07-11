\ BoxLambda
\ Anonymous lists

\ Start an anonymous list, e.g. l{ 4 ,  5 , 6 ... 
( -- )
: l{ 
  postpone [ ( )
  postpone ahead ( ahead structmatchconst )
  here ( ahead structmatchconst liststart )
  [immediate] [compileonly] 
;

\ Terminate an anonymous list and get its address and size (in cells).
\ E.g. ... 8 , 9 }l
( -- addr len )
: }l ( ahead structmatchconst liststart listitem )
  , ( ahead structmatchconst liststart ) 
  here ( ahead structmatchconst liststart listend )
  2swap postpone then ( liststart listend )
  unspan cell/ ( addr len )
  swap literal, literal, ( ) 
  ]
;

