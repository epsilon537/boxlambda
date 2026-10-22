0 variable (bpp)
<sheet-bitmap> bitmap 

: bitmap-pal-group-test

  true l0 layer-enable
  true display-enable

  l{ 2 , 4 , 8 }l
  [:
    (bpp) !

    ." bpp: " (bpp) @ . cr

    bitmap sheet{ 320 width 32 height (bpp) @ bpp }apply
    bitmap sheet.
    bitmap l0 layer-bitmap-mode
    bitmap pxl{ 0 0 vec2 xy 1 color }apply

    0 l0 layer-pal-group!
    ." pal-group: " l0 layer-pal-group@ . cr
    0 irqline!
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[1] rgb: " 1 pal@ hex. cr

    1 l0 layer-pal-group!
    ." pal-group: " l0 layer-pal-group@ . cr
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[17] rgb: " #17 pal@ hex. cr


    4 l0 layer-pal-group!
    ." pal-group: " l0 layer-pal-group@ . cr
    true line-capture-enable
    begin line-capture-enabled? not until
    ." pxl[0,0] capture: " 0 line-capture-pxl@ hex. cr
    ." palette[65] rgb: " #65 pal@ hex. cr
  ;] iter
;

[: bitmap-pal-group-test ;] &>file tst_dir/vera-bitmap-pal-group.log

s" tst_dir/vera-bitmap-pal-group.log" s" vera-bitmap-pal-group.ref" f_cmp ?assert

bitmap sheet-deinit
