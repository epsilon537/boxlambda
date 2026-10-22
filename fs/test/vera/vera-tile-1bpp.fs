0 variable w
0 variable h
<sheet-tileset> ts
<tmap> tm

: tile-1bpp-test

  true l0 layer-enable
  true display-enable

  tm tmap{ 32 width 32 height TMAP-TXT16 type }apply
  tm tmap.

  tm mapentry{ 0 0 vec2 xy 0 bg 1 fg 1 tidx }apply
  0 0 vec2 tm mapentry@ unpack-txt16
  s" mapentry[0,0]: %n bg %n fg %n tidx" printf cr

  l{ 8 , 16 }l
  [:
    w !
    l{ 8 , 16 }l
    [:
      h !
      ." width: " w @ . ."  height: " h @ . cr
      ts sheet{ w @ width h @ height 1 bpp 8 tiles }apply
      ts sheet.
      tm ts l0 layer-tilemap-mode
      l0 layer.
      ts pxl{ 1 tidx w @ 1- h @ 1- vec2 xy #1 color }apply
      ." pxl[w-1,h-1]: " ts pxl{ }get . cr
      h @ 1- irqline!
      true line-capture-enable
      begin line-capture-enabled? not until
      ." [w-1, h-1] capture: $" w @ 1- line-capture-pxl@ hex. cr
      ts pxl{ 1 tidx w @ 1- h @ 1- vec2 xy #0 color }apply
    ;] iter
  ;] iter
;

[: tile-1bpp-test ;] &>file tst_dir/vera-tile-1bpp.log

s" tst_dir/vera-tile-1bpp.log" s" vera-tile-1bpp.ref" f_cmp ?assert

tm tmap-deinit
ts sheet-deinit

