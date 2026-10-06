\ BoxLambda Forth

create fastboot-path 32 allot
s" /boxkern-forth.img" fastboot-path str>path ( ) \ Convert to C string

\ Fastboot helper Word:
\ Save forth imem and emem up to here to file boxkern-forth.img
( -- )
: fastboot-save
  compileto-save
  compiletoemem
  here fastboot-path forth-save-state ( )
  compileto-restore
  $00000001 SDRAM_BASE ! \ Indicate to host that compilation is complete
  ." Boxkern Forth compilation complete." cr
;




