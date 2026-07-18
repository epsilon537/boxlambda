0 variable w
0 variable h
<tileset> ts
[: l{ 320 , 640 }l
  [:
    w !
    l{ 32 , 64 }l
    [:
      h !
      ts tileset{ w @ width h @ height 1 bpp 1 tiles }set
      ts tileset :: print
      l0 layer{ ts tset 0 tidx }bitmap-mode
      ts pixel{ 0 tidx 0 0 xy #101 cidx }set
      ts pixel{ w @ 1- 0 xy }set
      ts pixel{ 0 h @ 1- 0 xy }set
      ts pixel{ w @ 1- h @ 1- xy }set
      0 irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ . cr
      1 line-capture :: pxl@ . cr
      w @ 2- line-capture :: pxl@ . cr
      w @ 1- line-capture :: pxl@ . cr
      1 irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ . cr
      1 line-capture :: pxl@ . cr
      w @ 2- line-capture :: pxl@ . cr
      w @ 1- line-capture :: pxl@ . cr
      h @ 1- irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ . cr
      1 line-capture :: pxl@ . cr
      w @ 2- line-capture :: pxl@ . cr
      w @ 1- line-capture :: pxl@ . cr
      h @ irq :: irqline-set
      true line-capture :: enable
      begin line-capture :: enabled? not until
      0 line-capture :: pxl@ . cr
      1 line-capture :: pxl@ . cr
      w @ 2- line-capture :: pxl@ . cr
      w @ 1- line-capture :: pxl@ . cr
      ts pixel{ 0 cidx }set
      ts pixel{ w @ 1- 0 xy }set
      ts pixel{ 0 h @ 1- 0 xy }set
      ts pixel{ w @ 1- h @ 1- xy }set
    ;] iter
  ;] iter
;] execute
