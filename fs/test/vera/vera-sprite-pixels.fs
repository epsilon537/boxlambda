0 variable w
0 variable h
0 variable b

<tset> ts
1 <spr> spr

: sprite-pixels

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  l{ 8 , 16 , 32 , 64 }l
  [:
    w !
    l{ 8 , 16 , 32 , 64 }l
    [:
      h !
      l{ 4 , 8 }l
      [:
        b !
        ts tset{ w @ width h @ height b @ bpp 8 tiles }set
        ts tset-print
        spr spr{ ts tset 2 tidx SPR-L1 z }set
        h @ 0 do
          w @ i 1+ min 0 ?do
            ts pxl{ 2 tidx i j vec2 xy CYAN color }set
          loop
        loop
        0 irqline!
        true line-capture-enable
        begin line-capture-enabled? not until
        ." x0 y0 capture: $" 0 line-capture-pxl@ hex. cr
        ." x1 y0 capture: $" 1 line-capture-pxl@ hex. cr
        h @ 1- irqline!
        true line-capture-enable
        begin line-capture-enabled? not until
        ." x0 yend-1 capture: $" 0 line-capture-pxl@ hex. cr
        ." x7 yend-1 capture: $" 7 line-capture-pxl@ hex. cr
        ." xend-1 yend-1 capture: $" w @ h @ min 1- line-capture-pxl@ hex. cr
        ." xend yend-1 capture: $" w @ h @ min line-capture-pxl@ hex. cr
        h @ irqline!
        true line-capture-enable
        begin line-capture-enabled? not until
        ." x0  yend capture: $" 0 line-capture-pxl@ hex. cr
        ." x7 yend capture: $" 7 line-capture-pxl@ hex. cr
        ." xend-1 yend capture: $" w @ h @ min 1- line-capture-pxl@ hex. cr
        ." xend yend capture: $" w @ h @ min line-capture-pxl@ hex. cr
        h @ 0 do
          w @ i 1+ min 0 ?do
            ts pxl{ 2 tidx i j vec2 xy 0 color }set
          loop
        loop
      ;] iter
    ;] iter
  ;] iter
;

[: sprite-pixels ;] &>file tst_dir/vera-sprite-pixels.log

s" tst_dir/vera-sprite-pixels.log" s" vera-sprite-pixels.ref" f_cmp ?assert

ts tset-deinit

