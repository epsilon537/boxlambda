\ BoxLambda Forth

\ array

\ Cell array
: array ( n -- ) ( i -- addr)
     create cells allot
     does> swap cells+ ;

\ Byte array
: carray ( n -- ) ( i -- addr)
     create cells allot
     does> swap + ;

