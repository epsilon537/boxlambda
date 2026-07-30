0 variable w
0 variable h
<tset> ts
<tmap> tm

: tile-1bpp-test

  true l0 layer-enable
  true display-enable

  tm tmap{ 32 width 32 height TMAP-TXT16 type }set
  tm tmap-print

  mapentry{ tm tmap 0 0 vec2 xy 0 bg 1 fg 1 tidx }set
  0 0 vec2 tm mapentry@ unpack-txt16
  s" mapentry[0,0]: %n bg %n fg %n tidx" printf cr

  l{ 8 , 16 }l
  [:
    w !
    l{ 8 , 16 }l
    [:
      h !
      ." width: " w @ . ."  height: " h @ . cr
      ts tset{ w @ width h @ height 1 bpp 8 tiles }set
      ts tset-print
      l0 layer{ ts tset tm tmap }tilemap-mode
      l0 layer-print
      pxl{ ts tset 1 tidx w @ 1- h @ 1- vec2 xy #1 color }set
      ." pxl[w-1,h-1]: " pxl{ }get . cr
      h @ 1- irqline-set
      true line-capture-enable
      begin line-capture-enabled? not until
      ." [w-1, h-1] capture: $" w @ 1- line-capture-pxl@ hex. cr
      pxl{ ts tset 1 tidx w @ 1- h @ 1- vec2 xy #0 color }set
    ;] iter
  ;] iter
;

[: tile-1bpp-test ;] &>file tst_dir/vera-tile-1bpp.log

s" tst_dir/vera-tile-1bpp.log" s" vera-tile-1bpp.ref" f_cmp ?assert

ts tset-deinit

