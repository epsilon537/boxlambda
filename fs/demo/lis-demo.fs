include /forth/fixpt-math-lib.fs
include /demo/font-loader.fs

320 constant XRES
240 constant YRES

compileto-save
compiletoimem

<tset> tsb
<tset> tsc
<tmap> tm

create sin-table 256 cells allot

: calc-sin-table
  256 0 do
    0 i 360,0 f* 256,0 f/  \ a=i*360/256
    sin 100,0 f*           \ 100*sin(a)
    0,5 d+ nip             \ round to int
    sin-table i cells + !
  loop
;

calc-sin-table

0 variable x
0 variable y
$10000 variable xf
$10000 variable yf
64 variable ph
0 variable frametoggle

( y addr len -- )
: txt-line
  0 do ( y addr )
    over i swap vec2 ( y addr vec2 )
    over i + c@ $20 - ( y addr vec2 tidx )
    tm mapentry{ ( tidx ) tidx ( vec2 ) xy YELLOW fg BLACK bg }apply ( y addr )
  loop
  2drop
;

: draw-xf-yf
  40 [:
    >r 
    yf @ 12 rshift xf @ 12 rshift s" x: %n y: %n                " r@ sprintf
    #1 -rot txt-line
    r> 
  ;] with-temp-allot
;

: keyctrl
  key? if
    key dup case
      #27 of quit endof \ ESC

      [char] x of 
        xf @ $100000 < if
          $1000 xf +!
        then
      endof

      [char] y of 
        yf @ $100000 < if
          $1000 yf +!
        then
      endof

      [char] X of 
        xf @ $1000 > if
          -$1000 xf +!
        then
      endof

      [char] Y of 
        yf @ $1000 > if
          -$1000 yf +!
        then
      endof
    endcase

    draw-xf-yf
  then
;

: lis-demo

  tsb tset{ XRES width YRES height 1 bpp 2 tiles }apply
  tsb tset-print
  l0 layer{ tsb tset 0 tidx }bitmap-mode
  l0 layer-print

  tsc tset{ 8 width 8 height 1 bpp 256 tiles }apply
  tm tmap{ 64 width 32 height TMAP-TXT16 type }apply
  tsc tset-print
  tm tmap-print

  l1 layer{ tsc tset tm tmap }tilemap-mode
  l1 layer-print

  tsc s" night-in-tokyo.fnt" load-font

  true l0 layer-enable
  true l1 layer-enable
  false sprites-enable

  \ scale to 320x240
  $40 dup hscale! vscale!

  true display-enable
 
  #29 s"  Press x/X/y/Y to adjust frequencies." ( addr len )
  txt-line

  $10000 xf !
  $10000 yf !

  draw-xf-yf

  begin
    frametoggle @ 1 xor frametoggle !
    frametoggle @ tsb tset-tidx>addr tsb tset-tilesize@ 0 fill

    tsb pxl{ frametoggle @ tidx WHITE color }set

    ph @
    256 0 do
      i xf @ * 16 rshift 255 and cells sin-table + @ 160 + ( ph x )
      over i yf @ * 16 rshift + 255 and cells sin-table + @ 120 + ( ph x y )
      vec2 ( ph vec2 )
      tsb pxl{ ( vec2 ) xy }apply ( ph )
    2 +loop
    drop ( )
 
    ph @ 1 + 255 and ph !

    begin scanline@ 470 >= until

    l0 layer{ tsb tset frametoggle @ tidx }bitmap-mode

    keyctrl

  again
;

compileto-restore

lis-demo

