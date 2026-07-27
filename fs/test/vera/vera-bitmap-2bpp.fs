0 variable w
0 variable h
<tset> ts

: bitmap-2bpp-test

  true l0 layer-enable
  true display-enable

  l{ 320 , 640 }l
  [:
    w !
    l{ 32 , 64 }l
    [:
      h !
      ts tset{ w @ width h @ height 2 bpp 1 tiles }set
      ts tset-print
      l0 layer{ ts tset 0 tidx }bitmap-mode
      pxl{ ts tset 0 tidx 0 0 vec2 xy #103 color }set
      pxl{ w @ 1- 0 vec2 xy }set
      pxl{ 0 h @ 1- vec2 xy }set
      pxl{ w @ 1- h @ 1- vec2 xy }set
      0 irqline-set
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      1 irqline-set
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      h @ 1- irqline-set
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      h @ irqline-set
      true line-capture-enable
      begin line-capture-enabled? not until
      0 line-capture-pxl@ hex. cr
      1 line-capture-pxl@ hex. cr
      w @ 2- line-capture-pxl@ hex. cr
      w @ 1- line-capture-pxl@ hex. cr
      pxl{ ts tset 0 0 vec2 xy 0 color }set
      pxl{ w @ 1- 0 vec2 xy }set
      pxl{ 0 h @ 1- vec2 xy }set
      pxl{ w @ 1- h @ 1- vec2 xy }set
    ;] iter
  ;] iter
;

[: bitmap-2bpp-test ;] &>file tst_dir/vera-bitmap-2bpp.log

s" tst_dir/vera-bitmap-2bpp.log" s" vera-bitmap-2bpp.ref" f_cmp ?assert

ts tset-deinit

