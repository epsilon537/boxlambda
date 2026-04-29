\ BoxLambda Forth
\ Idea similar to http://lars.nocrew.org/dpans/dpansa15.htm#A.15.6.2.2532

: nexttoken ( -- addr len )
  begin
    token         \ Fetch new token.
  dup 0= while    \ If length of token is zero, end of line is reached.
    2drop refill  \ Request a next line.
  repeat
;

: [else] ( -- )
  1 \ Initial level of nesting
  begin ( level )
    nexttoken ( level addr len )

    2dup s" [if]"     compare
 >r 2dup s" [ifdef]"  compare r> or
 >r 2dup s" [ifndef]" compare r> or ( level addr len f )

    if
      2drop 1+  ( level+1 ) \ One more level of nesting
    else
      2dup s" [else]" compare ( level addr len f )
      if
        2drop 1- ( level-1 )
        dup if
          1+ ( level )
        then  \ Finished if [else] is reached in level 1. Skip [else] branch otherwise.
      else ( level addr len )
        s" [then]"
        compare ( level f )
        if 1- ( level-1)
        then  \ Level completed.
      then
    then

    ?dup 0=
  until

  [immediate] [0-foldable]
;

: [then] ( -- ) [immediate] [0-foldable] ;

: [if]   ( ? -- )                 0=  if postpone [else] then [immediate] [1-foldable] ;
: [ifdef]  ( -- ) token find drop 0=  if postpone [else] then [immediate] [0-foldable] ;
: [ifndef] ( -- ) token find drop 0<> if postpone [else] then [immediate] [0-foldable] ;

