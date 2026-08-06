0 variable (bpp)
<tset> ts

: bitmap-paloffset-test

  true l0 layer-enable
  true display-enable

  l{ 2 , 4 , 8 }l
  [:
    (bpp) !

    ." bpp: " (bpp) @ . cr

    ts tset{ 320 width 32 height (bpp) @ bpp 1 tiles }set
    ts tset-print
    l0 layer{ ts tset 0 tidx }bitmap-mode
    ts pxl{ 0 tidx 0 0 vec2 xy 1 color }set

    0 l0 layer-paloffset!
    ." paloffset: " l0 layer-paloffset@ . cr
    0 irqline!
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[1] rgb: " 1 pal@ hex. cr

    1 l0 layer-paloffset!
    ." paloffset: " l0 layer-paloffset@ . cr
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[17] rgb: " #17 pal@ hex. cr


    4 l0 layer-paloffset!
    ." paloffset: " l0 layer-paloffset@ . cr
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[65] rgb: " #65 pal@ hex. cr
  ;] iter
;

[: bitmap-paloffset-test ;] &>file tst_dir/vera-bitmap-paloffset.log

s" tst_dir/vera-bitmap-paloffset.log" s" vera-bitmap-paloffset.ref" f_cmp ?assert

ts tset-deinit

