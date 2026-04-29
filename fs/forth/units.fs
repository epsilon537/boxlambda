\ BoxLambda Forth
\
\ Units.

\ 1 cell size
4 constant cell

\ Add the size of n cells to x
\ ( x n -- x+n*cell )
: cells+ cell * + ;

\ chars and char+
: chars ( u -- u ) [0-foldable] ;
: char+ ( a -- a+1 ) 1+ [1-foldable] ;

\ max and min int values
0 invert                    constant max-uint
0 invert 1 rshift           constant max-int
0 invert 1 rshift invert    constant min-int

