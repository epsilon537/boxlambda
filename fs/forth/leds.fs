\ BoxLambda Forth
\ LED driver

: led! ( mask "name" -- )
  create
    \ compile-time: store the parameter in the dictionary body
    ,
  does> ( f pfa )
    @ ( f mask )
    swap if ( mask )
      GPIO_RGPIO_OUT_ADDR @ or GPIO_RGPIO_OUT_ADDR ! ( )
    else
      not GPIO_RGPIO_OUT_ADDR @ and GPIO_RGPIO_OUT_ADDR ! ( )
    then
;

: led@ ( mask "name" -- )
  create
    \ compile-time: store the parameter in the dictionary body
    ,
  does> ( pfa )
    @ ( mask )
    GPIO_RGPIO_OUT_ADDR @ and 0<>
;


1 led! led-keyboard-detect!
2 led! led-5!
4 led! led-6!
8 led! led-7!

1 led@ led-keyboard-detect@
2 led@ led-5@
4 led@ led-6@
8 led@ led-7@

