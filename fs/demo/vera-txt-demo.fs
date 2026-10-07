\ BoxLambda Forth
\ A very basic VERA text mode demo. Renders entered characters on a 320x240 screen configured
\ in 8x8 text mode.

true include-verbose !

vram-reset

include /demo/font-loader.fs

320 constant XRES
240 constant YRES
XRES 8 / constant #COLS
YRES 8 / constant #ROWS

\ Tileset for the font, holding the font definition (pixel data).
<sheet-tileset> ts
\ Tileset for the cursor sprite, holding the sprite pixel data.
<sheet-tileset> ts-spr
\ Tilemap, i.e. the on-screen grid of 40x30 characters.
<tmap> tm
\ The cursor sprite object, holding sprite position etc.
0 <spr> spr
\ The cursor position as a vec2 object.
0 0 vec2 variable cursor

\ Backspace handler.
( -- )
: (BS)
  \ Update cursor position.
  cursor @ vec2.xy ( x y )
  swap ( y x )
  dup if ( y x )
    1- ( y x-1 )
  else
    drop
    dup if ( y )
      1- ( y-1 )
      #COLS 1- ( y x )
    else
      drop 0 0 ( y x )
    then
  then
  swap vec2 cursor ! ( )
  \ Erase char at cursor
  tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }apply
;

\ Carriage Return handler.
( -- )
: (CR)
  \ Update cursors position.
  cursor @ vec2.y 1+ ( y )
  dup #ROWS = if ( y )
    drop 0 ( y )
  then
  0 swap vec2 cursor ! ( )
  \ Erase char at cursor
  tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }apply
;

\ Printable key handler.
( key -- )
: (printable-key)
  $20 - ( key tidx )
  \ Draw the character at the cursor position
  tm mapentry{ ( tidx ) tidx cursor @ xy YELLOW fg BLACK bg }apply
  \ Update cursor position
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
;

\ Called from the edit loop. Process one keypress. Returns character entered.
( -- c )
: (edit-1)
  [ 0 1 stack-checker ]
  key dup case
    #27 of endof \ ESC
    #8 of (BS) endof
    #13 of (CR) endof
    (printable-key)
  endcase ( key )

  \ Draw the cursor
  spr spr{ cursor @ 8 * xy }apply
;

: (init-spr-tile)
  8 0 do
    ts-spr pxl{ 0 tidx }set
    ts-spr pxl{ i 0 vec2 xy BLACK color }apply
    ts-spr pxl{ i 1 vec2 xy }apply
    ts-spr pxl{ i 2 vec2 xy }apply
    ts-spr pxl{ i 3 vec2 xy }apply
    ts-spr pxl{ i 4 vec2 xy }apply
    ts-spr pxl{ i 5 vec2 xy }apply
    ts-spr pxl{ i 6 vec2 xy WHITE color }apply
    ts-spr pxl{ i 7 vec2 xy }apply
  loop
;

\ The VERA text demo entry point.
( -- )
: vera-txt-demo
  0 0 vec2 cursor ! \ Set initial cursor position to upper left corner.
  ts sheet{ 8 width 8 height 1 bpp 256 tiles }apply \ Create tileset object for font.
  ts-spr sheet{ 8 width 8 height 4 bpp 1 tiles }apply \ Create tileset object for cursor sprite.
  (init-spr-tile) \ Generate the sprite pixel data.
  \ Create the tilemap object. 64 and 32 are the most suitable accepted width and height values
  \ to accommodate a 40x30 grid.
  tm tmap{ 64 width 32 height TMAP-TXT16 type }apply
  spr spr{ ts-spr sheet 0 tidx SPR-L0-L1 z cursor @ 8 * xy }apply \ Create the cursor sprite object.

  tm ts l0 layer-tilemap-mode \ Using layer 0 in tilemap mode.

  cr
  ts token load-font \ Load the font into the tileset (see font-loader.fs).
  ." #glyphs: " . cr

  true l0 layer-enable
  false l1 layer-enable
  true sprites-enable

  \ 0.5 scale factor to scale from to 320x240 resolution.
  $40 dup hscale! vscale!

  true display-enable

  \ Print diagnostic info of the VERA objects we just created.
  ts sheet.
  tm tmap.
  l0 layer.
  spr spr.

  cr
  ." VERA Text Mode Demo" cr
  ." -------------------" cr
  ." Enter some text. It should appear on the VGA display." cr
  ." <Enter> and <BackSpace> should also work. Cursor keys not yet." cr
  ." <ESC> exits the demo." cr

  \ Editing loop
  begin 
    (edit-1) dup . \ Process one keypress.
    27 = if exit then \ Exit it ESC is pressed.
    100000 0 do loop \ delay
  again
;

\ Start the demo using night-in-tokyo font.
vera-txt-demo night-in-tokyo.fnt

