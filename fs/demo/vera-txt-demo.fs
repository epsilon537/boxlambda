include /demo/font-loader.fs

320 constant XRES
240 constant YRES
XRES 8 / constant #COLS
YRES 8 / constant #ROWS

<tset> ts
<tset> ts-spr
<tmap> tm
0 <spr> spr

0 0 vec2 variable cursor

( -- c )
: (edit-1)
  [ 0 1 stack-checker ]
  key dup case
    #27 of endof \ ESC

    #8 of \ BS
      cursor @ vec2.xy ( key x y )
      swap ( key y x )
      dup if ( key y x )
        1- ( key y x-1 )
      else
        drop
        dup if ( key y )
          1- ( key y-1 )
          #COLS 1- ( key y x )
        else
          drop 0 0 ( key y x )
        then
      then
      swap vec2 cursor ! ( key )
      \ Erase char at cursor
      tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }set
    endof

    #13 of 
      cursor @ vec2.y 1+ ( key y )
      dup #ROWS = if ( key y )
        drop 0 ( key y )
      then
      0 swap vec2 cursor ! ( key )
      \ Erase char at cursor
      tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }set
    endof

    $20 - ( key tidx )
    \ draw the character at the cursor position
    tm mapentry{ ( tidx ) tidx cursor @ xy YELLOW fg BLACK bg }set
    cursor @ vec2.xy swap 1+ swap ( key x y )
    over #COLS = if ( key x y )
      nip 0 swap ( key x y )
      1+ ( key x y )
      dup #ROWS = if ( key x y )
        drop 0 ( key x y )
      then
    then
    vec2 cursor ! ( key )
    dup
  endcase

  \ Draw the cursor
  spr spr{ cursor @ 8 * xy }set
;

: (init-spr-tile)
  8 0 do
    ts-spr pxl{ 0 tidx i 0 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 1 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 2 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 3 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 4 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 5 vec2 xy BLACK color }set
    ts-spr pxl{ 0 tidx i 6 vec2 xy WHITE color }set
    ts-spr pxl{ 0 tidx i 7 vec2 xy WHITE color }set
  loop
;

( -- )
: vera-txt-demo
  0 0 vec2 cursor !
  ts tset{ 8 width 8 height 1 bpp 256 tiles }set
  ts-spr tset{ 8 width 8 height 4 bpp 1 tiles }set
  (init-spr-tile)
  tm tmap{ 64 width 32 height TMAP-TXT16 type }set
  spr spr{ ts-spr tset 0 tidx SPR-L0-L1 z cursor @ 8 * xy }set

  l0 layer{ ts tset tm tmap }tilemap-mode

  cr
  ts token load-font
  ." #glyphs: " . cr

  true l0 layer-enable
  false l1 layer-enable
  true sprites-enable

  \ scale to 320x240
  $40 dup hscale! vscale!

  true display-enable

  ts tset-print
  tm tmap-print
  l0 layer-print

  begin 
    (edit-1) dup .
    27 = if exit then
    100000 0 do loop \ delay
  again
;

vera-txt-demo night-in-tokyo.fnt

