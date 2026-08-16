0 variable w
0 variable h
<tset> ts

: bitmap-8bpp-test

  true l0 layer-enable
  false l1 layer-enable
  true display-enable
  false sprites-enable

  vram :: reset

  l{ 320 , 640 }l
  [:
    w !
    l{ 32 , 64 }l
    [:
      h !
      ts tset{ w @ width h @ height 8 bpp 1 tiles }set
      ts tset-print
      l0 layer{ ts tset 0 tidx }bitmap-mode
      ts pxl{ 0 tidx 0 0 vec2 xy #255 color }set
      ts pxl{ w @ 1- 0 vec2 xy }set
      ts pxl{ 0 h @ 1- vec2 xy }set
      ts pxl{ w @ 1- h @ 1- vec2 xy }set
      0 irqline!
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      1 irqline!
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      h @ 1- irqline!
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      h @ irqline!
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      ts pxl{ 0 0 vec2 xy 0 color }set
      ts pxl{ w @ 1- 0 vec2 xy }set
      ts pxl{ 0 h @ 1- vec2 xy }set
      ts pxl{ w @ 1- h @ 1- vec2 xy }set
    ;] iter
  ;] iter
;

[: bitmap-8bpp-test ;] &>file tst_dir/vera-bitmap-8bpp.log

s" tst_dir/vera-bitmap-8bpp.log" s" vera-bitmap-8bpp.ref" f_cmp ?assert

ts tset-deinit

