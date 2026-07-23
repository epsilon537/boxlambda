\ BoxLambda Forth
\ Register bitfield accessors

: bitfield@ ( address mask offset "name" -- )
    create
        \ compile-time: store the parameters into the dictionary body
        , , ,
    does>
        dup @           \ fetch offset            ( pfa offset )
        swap cell+ dup @ \ fetch mask             ( offset pfa+ mask )
        swap cell+ @    \ fetch register address  ( offset mask addr )
        @               \ read raw register value ( offset mask val )
        and             \ apply mask              ( offset masked-val )
        swap rshift      \ shift right by offset  ( final-bitfield-value )
;

: bitfield! ( address mask offset "name" -- )
    create
        \ compile-time: store parameters in dictionary
        , , ,

    does> ( v pfa )
        dup @              \ offset            ( v pfa offset )
        swap cell+ dup @   \ mask              ( v offset pfa+ mask )
        swap cell+ @ >r    \ address           ( v offset mask R: addr )
        -rot lshift        \ v<<offset         ( mask vshifted R: addr )
        over and           \ vshifted&mask     ( mask vshiftedmasked R: addr )
        r@ @               \ read register     ( mask vshifted old R: addr )
        rot bic or         \                   ( new R: addr ) 
        r> !               \                   ( )
;

: bitfield16@ ( mask offset "name" -- )
    create
        \ compile-time: store the parameters into the dictionary body
        h, h,
    does>
        ( address pfa -- value )
        dup h@          \ fetch offset            ( addr pfa offset )
        swap 2 + h@     \ fetch mask              ( addr offset mask )
        rot h@          \ read raw register value ( offset mask val )
        and             \ apply mask              ( offset masked-val )
        swap rshift     \ shift right by offset   ( final-bitfield-value )
;

: bitfield16! ( mask offset "name" -- )
    create \ compile-time: store parameters in dictionary
        h, h,
    does>
        ( bitfield-v addr pfa )
        dup h@          \ offset               ( bitfieldv addr pfa offset )
        swap 2 + h@     \ mask                 ( bitfieldv addr offset mask )
        rot >r                                 ( bitfieldv offset mask R: addr )
        r@ h@                                  ( bitfieldv offset mask val R: addr )
        swap bic                               ( bitfieldv offset valmasked R: addr )
        -rot                                   ( valmasked bitfieldv offset R: addr )
        lshift or                              ( newval R: addr )
        r> h!                                  ( )
;

