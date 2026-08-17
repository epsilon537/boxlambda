( -- )
: fastboot-save
  compileto-save
  compiletoemem
  \ save forth imem and emem up to here to following file:
  256 [: ( buf )
    >r
    s" /boxkern-forth.img" ( addr len R: buf )
    r@ str>path ( R: buf ) \ Convert to C string
    here r> forth-save-state ( )
  ;] with-temp-allot
  compileto-restore
  $00000001 SDRAM_BASE ! \ Indicate to host that compilation is complete
  ." Boxkern Forth compilation complete." cr
;




