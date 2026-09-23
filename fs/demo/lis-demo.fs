\ BoxLambda Forth
\ Lissajous curves VERA bitmap mode demo.

." Compiling demo, will take a few seconds..." cr

vram-reset

include /demo/font-loader.fs

320 constant XRES
240 constant YRES

compileto-save
compiletoimem

<tset> tsb \ bitmap tileset
<tset> tsc \ font tileset
<tmap> tm \ Text grid tilemap

create sin-table \ 256 cells
0  , 2  , 5  , 7  , 10  , 12  , 15  , 17  ,
20  , 22  , 24  , 27  , 29  , 31  , 34  , 36  ,
38  , 41  , 43  , 45  , 47  , 49  , 51  , 53  ,
56  , 58  , 60  , 62  , 63  , 65  , 67  , 69  ,
71  , 72  , 74  , 76  , 77  , 79  , 80  , 82  ,
83  , 84  , 86  , 87  , 88  , 89  , 90  , 91  ,
92  , 93  , 94  , 95  , 96  , 96  , 97  , 98  ,
98  , 99  , 99  , 99  , 100  , 100  , 100  , 100  ,
100  , 100  , 100  , 100  , 100  , 99  , 99  , 99  ,
98  , 98  , 97  , 96  , 96  , 95  , 94  , 93  ,
92  , 91  , 90  , 89  , 88  , 87  , 86  , 84  ,
83  , 82  , 80  , 79  , 77  , 76  , 74  , 72  ,
71  , 69  , 67  , 65  , 63  , 62  , 60  , 58  ,
56  , 53  , 51  , 49  , 47  , 45  , 43  , 41  ,
38  , 36  , 34  , 31  , 29  , 27  , 24  , 22  ,
20  , 17  , 15  , 12  , 10  , 7  , 5  , 2  ,
0  , -2  , -5  , -7  , -10  , -12  , -15  , -17  ,
-20  , -22  , -24  , -27  , -29  , -31  , -34  , -36  ,
-38  , -41  , -43  , -45  , -47  , -49  , -51  , -53  ,
-56  , -58  , -60  , -62  , -63  , -65  , -67  , -69  ,
-71  , -72  , -74  , -76  , -77  , -79  , -80  , -82  ,
-83  , -84  , -86  , -87  , -88  , -89  , -90  , -91  ,
-92  , -93  , -94  , -95  , -96  , -96  , -97  , -98  ,
-98  , -99  , -99  , -99  , -100  , -100  , -100  , -100  ,
-100  , -100  , -100  , -100  , -100  , -99  , -99  , -99  ,
-98  , -98  , -97  , -96  , -96  , -95  , -94  , -93  ,
-92  , -91  , -90  , -89  , -88  , -87  , -86  , -84  ,
-83  , -82  , -80  , -79  , -77  , -76  , -74  , -72  ,
-71  , -69  , -67  , -65  , -63  , -62  , -60  , -58  ,
-56  , -53  , -51  , -49  , -47  , -45  , -43  , -41  ,
-38  , -36  , -34  , -31  , -29  , -27  , -24  , -22  ,
-20  , -17  , -15  , -12  , -10  , -7  , -5  , -2  ,

0 variable x
0 variable y
$10000 variable xf \ x frequency
$10000 variable yf \ y frequency
64 variable ph \ phase difference between x and y.
0 variable frametoggle \ toggle for rendering to bitmap 0 or 1 for double buffering.

\ Draw a line of text at give y position.
( y addr len -- )
: txt-line
  0 do ( y addr )
    over i swap vec2 ( y addr vec2 )
    over i + c@ $20 - ( y addr vec2 tidx )
    tm mapentry{ ( tidx ) tidx ( vec2 ) xy YELLOW fg BLACK bg }apply ( y addr )
  loop
  2drop
;

\ Print on xf and yf values on screen.
( -- )
: draw-xf-yf
  40 [:
    >r 
    yf @ 12 rshift xf @ 12 rshift s" x: %n y: %n                " r@ sprintf
    #1 -rot txt-line
    r> 
  ;] with-temp-allot
;

\ key handler
( -- )
: keyctrl
  key? if
    key dup case
      #27 of quit endof \ ESC

      [char] x of 
        xf @ $100000 < if $1000 xf +! then
      endof

      [char] y of 
        yf @ $100000 < if $1000 yf +! then
      endof

      [char] X of 
        xf @ $1000 > if -$1000 xf +! then
      endof

      [char] Y of 
        yf @ $1000 > if -$1000 yf +! then
      endof
    endcase

    draw-xf-yf
  then
;

\ The main loop, invoked from list-demo below.
( -- )
: drawloop
  begin
    \ double bufferin toggle
    frametoggle @ 1 xor frametoggle !
    frametoggle @ tsb tset-tidx>addr tsb tset-tilesize@ 0 fill \ Erase the bitmap

    tsb pxl{ frametoggle @ tidx WHITE color }set \ Set pixel color to white

    \ Draw 256 points
    ph @
    256 0 do
      \ sin(i*xf), modulo 256 to wraparound the sin-table
      i xf @ * 16 rshift 255 and cells sin-table + @ 160 + ( ph x )
      \ sin((i*yf+ph))
      over i yf @ * 16 rshift + 255 and cells sin-table + @ 120 + ( ph x y )
      vec2 ( ph vec2 )
      tsb pxl{ ( vec2 ) xy }apply ( ph )
    2 +loop
    drop ( )
 
    \ Increment phase to create an animation.
    ph @ 1 + 255 and ph !

    \ Double buffer switch.
    begin scanline@ 470 >= until
    l0 layer{ tsb tset frametoggle @ tidx }bitmap-mode

    keyctrl
  again
;

\ Demo entry point
( -- )
: lis-demo

  tsb tset{ XRES width YRES height 1 bpp 2 tiles }apply \ tileset of 2 bitmaps for double buffering.
  tsb tset.
  l0 layer{ tsb tset 0 tidx }bitmap-mode
  l0 layer.

  tsc tset{ 8 width 8 height 1 bpp 256 tiles }apply \ tileset for the font.
  tm tmap{ 64 width 32 height TMAP-TXT16 type }apply \ text grid tile map.
  tsc tset.
  tm tmap.

  l1 layer{ tsc tset tm tmap }tilemap-mode
  l1 layer.

  tsc s" night-in-tokyo.fnt" load-font \ load the font into the tileset. See font-loader.fs.

  true l0 layer-enable
  true l1 layer-enable
  false sprites-enable

  $40 dup hscale! vscale! \ scale to 320x240

  true display-enable
 
  ." Rendering Lissajous animation on VGA display. Press <ESC> to quit." cr

  \ Print on screen handy help message for the user.
  #29 s"  Press x/X/y/Y to adjust frequencies." ( addr len )
  txt-line

  $10000 xf ! \ .16 fixed point
  $10000 yf !

  draw-xf-yf
  drawloop
;

compileto-restore

lis-demo

