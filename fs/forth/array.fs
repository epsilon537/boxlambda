\ BoxLambda Forth

\ array

\ Cell array
: array ( n -- ) ( i -- addr)
     create cells allot
     does> swap cells+ ;

\ Halfword array
: harray ( n -- ) ( i -- addr)
     create cells allot
     does> swap 2* + ;

\ Byte array
: carray ( n -- ) ( i -- addr)
     create cells allot
     does> swap + ;

