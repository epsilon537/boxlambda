\ BoxLambda Forth

\ Range-related words

\ Convert addr len to start-end address span.
: span ( addr len -- start end )
  over + ;

\ Convert add len to end-start address span.
: bounds ( addr len -- end start )
  over + swap ;

\ True if n is within the range [low..high[
: within ( n low high -- flag ) over - >r - r> u< [3-foldable] ;

