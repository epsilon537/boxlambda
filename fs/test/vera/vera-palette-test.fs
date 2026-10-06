<sheet-bitmap> bitmap

: palette-test

  true l0 layer-enable
  true display-enable

  bitmap sheet{ #320 width #32 height 8 bpp }apply
  bitmap sheet.
  bitmap l0 layer-bitmap-mode
  l0 layer.
  bitmap pxl{ 0 0 vec2 xy #255 color }apply
  ." pxl 0,0 : " bitmap pxl{ 0 0 vec2 xy }get . cr
  ." pxl 1,0 : " bitmap pxl{ 1 0 vec2 xy }get . cr

  0 irqline!

  true line-capture-enable
  begin line-capture-enabled? not until
  ." Default palette line capture" cr
  0 line-capture-pxl@ hex. cr
  1 line-capture-pxl@ hex. cr
  cr

  $321 0 pal!
  $123 #255 pal!
  ." Modified palette:" cr
  ." pal 0: " 0 pal@ hex. cr
  ." pal 255: " #255 pal@ hex. cr

  true line-capture-enable
  begin line-capture-enabled? not until
  0 line-capture-pxl@ hex. cr
  1 line-capture-pxl@ hex. cr
  cr

  ." Restored palette:" cr
  pal-init
  ." pal 0: " 0 pal@ hex. cr
  ." pal 255: " #255 pal@ hex. cr
  0 line-capture-pxl@ hex. cr
  1 line-capture-pxl@ hex. cr
;

[: palette-test ;] &>file tst_dir/vera-palette-test.log

s" tst_dir/vera-palette-test.log" s" vera-palette-test.ref" f_cmp ?assert

bitmap sheet-deinit

