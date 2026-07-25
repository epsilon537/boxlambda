0 variable w
0 variable h
<tileset> ts

: bitmap-1bpp-test

  true l0 layer :: enable
  true display-enable

  l{ 320 , 640 }l
  [:
    w !
    l{ 32 , 64 }l
    [:
      h !
      ts tileset{ w @ width h @ height 1 bpp 1 tiles }set
      ts tileset :: print
      l0 layer{ ts tset 0 tidx }bitmap-mode
      pixel{ ts tset 0 tidx 0 0 vec2 xy #101 color }set
      pixel{ w @ 1- 0 vec2 xy }set
      pixel{ 0 h @ 1- vec2 xy }set
      pixel{ w @ 1- h @ 1- vec2 xy }set
      0 irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ hex. cr
      1 line-capture :: pxl@ hex. cr
      w @ 2- line-capture :: pxl@ hex. cr
      w @ 1- line-capture :: pxl@ hex. cr
      1 irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ hex. cr
      1 line-capture :: pxl@ hex. cr
      w @ 2- line-capture :: pxl@ hex. cr
      w @ 1- line-capture :: pxl@ hex. cr
      h @ 1- irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ hex. cr
      1 line-capture :: pxl@ hex. cr
      w @ 2- line-capture :: pxl@ hex. cr
      w @ 1- line-capture :: pxl@ hex. cr
      h @ irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ hex. cr
      1 line-capture :: pxl@ hex. cr
      w @ 2- line-capture :: pxl@ hex. cr
      w @ 1- line-capture :: pxl@ hex. cr
      pixel{ ts tset 0 0 vec2 xy 0 color }set
      pixel{ w @ 1- 0 vec2 xy }set
      pixel{ 0 h @ 1- vec2 xy }set
      pixel{ w @ 1- h @ 1- vec2 xy }set
    ;] iter
  ;] iter
;

[: bitmap-1bpp-test ;] &>file tst_dir/vera-bitmap-1bpp.log

s" tst_dir/vera-bitmap-1bpp.log" s" vera-bitmap-1bpp.ref" f_cmp ?assert

ts tileset :: deinit

