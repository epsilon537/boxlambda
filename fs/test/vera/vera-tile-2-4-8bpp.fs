0 variable w
0 variable h
0 variable b

<tset> ts
<tmap> tm

: tile-2-4-8bpp-test

  true l0 layer-enable
  true display-enable

  tm tmap{ 32 width 32 height TMAP-TILE type }apply
  tm tmap-print

  tm mapentry{ 0 0 vec2 xy 0 bg 1 fg 1 tidx }apply
  0 0 vec2 tm mapentry@ unpack-txt16
  s" mapentry[0,0]: %n bg %n fg %n tidx" printf cr

  l{ 8 , 16 }l
  [:
    w !
    l{ 8 , 16 }l
    [:
      h !
      l{ 2 , 4 , 8 }l
      [:
        b !
        ts tset{ w @ width h @ height b @ bpp 8 tiles }apply
        ts tset-print
        l0 layer{ ts tset tm tmap }tilemap-mode
        l0 layer-print
        ts pxl{ 1 tidx w @ 1- h @ 1- vec2 xy #101 color }apply
        ." pxl[w-1,h-1]: " ts pxl{ }get . cr
        h @ 1- irqline!
        true line-capture-enable
        begin line-capture-enabled? not until
        ." [w-1, h-1] capture: $" w @ 1- line-capture-pxl@ hex. cr
        ts pxl{ 1 tidx w @ 1- h @ 1- vec2 xy #0 color }apply
      ;] iter
    ;] iter
  ;] iter
;

[: tile-2-4-8bpp-test ;] &>file tst_dir/vera-tile-2-4-8bpp.log

s" tst_dir/vera-tile-2-4-8bpp.log" s" vera-tile-2-4-8bpp.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

