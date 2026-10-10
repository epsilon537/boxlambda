\ BoxLambda Forth
\ Display the default palette organized into groups using 8x8 tiles in 256 color text mode.

true include-verbose !

vram-reset

include /demo/font-loader.fs

320 constant XRES
240 constant YRES
XRES 8 / constant #COLS
YRES 8 / constant #ROWS

\ Tileset for the font, holding the font definition (pixel data).
<sheet-tileset> ts
\ Tilemap, i.e. the on-screen grid of 40x30 characters.
<tmap> tm

\ Print a line of text at give y position.
( y addr len -- )
: txt-line
  0 do ( y addr )
    over i swap vec2 ( y addr vec2 )
    over i + c@ $20 - ( y addr vec2 tidx )
    tm mapentry{ ( tidx ) tidx ( vec2 ) xy WHITE fg BLACK bg }apply ( y addr )
  loop
  2drop
;

\ Render the text on the lines above the palette tiles.
( -- )
: render-txt
  0 s" Pal-Group 0 COMMODORE 64" txt-line
  2 s" Pal-Group 1 GRAYSCALE" txt-line
  4 s" Pal-Group 2 PICO-8" txt-line
  6 s" Pal-Group 3 MIYAZAKI 16" txt-line
  8 s" Pal-Group 4 SWEETIE 16" txt-line
  10 s" Pal-Group 5 VANILLA MILKSHAKE" txt-line
  12 s" Pal-Group 6 SARA 98C" txt-line
  14 s" Pal-Group 7 YUNO" txt-line
  16 s" Pal-Group 8 AAP SPLENDOR 128" txt-line
;

\ Render the palette tiles on the lines below the text
( -- )
: render-palette

  tm mapentry{ $80 tidx BLACK bg }set

  8 0 do
    16 0 do
      tm mapentry{ i j 2* 1+ vec2 xy i j 16 * + fg }apply
    loop
  loop

  8 0 do
    16 0 do
      tm mapentry{ i j 17 + vec2 xy i j 16 * + 128 + fg }apply
    loop
  loop
;

\ The VERA palette demo entry point.
( "font-file-name" -- )
: vera-palette-demo
  ts sheet{ 8 width 8 height 1 bpp 256 tiles }apply \ Create tileset object for font.
  \ Create the tilemap object. 64 and 32 are the most suitable accepted width and height values
  \ to accommodate a 40x30 grid.
  tm tmap{ 64 width 32 height TMAP-TXT256 type }apply

  tm ts l0 layer-tilemap-mode \ Using layer 0 in tilemap mode.

  cr
  ts token load-font \ Load the font into the tileset (see font-loader.fs).
  ." #glyphs: " . cr

  \ Generate an 'inverted space' at tidx=$80
  $80 ts sheet-tidx>addr 8 $ff fill

  true l0 layer-enable

  \ 0.5 scale factor to scale from to 320x240 resolution.
  $40 dup hscale! vscale!

  render-palette
  render-txt

  true display-enable

  \ Print diagnostic info of the VERA objects we just created.
  ts sheet.
  tm tmap.
  l0 layer.

  cr
  ." VERA Palette Demo" cr
  ." -----------------" cr
  ." <ESC> exits the demo." cr

  \ Editing loop
  begin 
    key \ Process one keypress.
    27 = if exit then \ Exit it ESC is pressed.
  again
;

\ Start the demo
vera-palette-demo district-digital.fnt

