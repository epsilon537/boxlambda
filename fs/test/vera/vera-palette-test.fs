<tset> ts

: palette-test

  true l0 layer-enable
  true display-enable

  ts tset{ #320 width #32 height 8 bpp 1 tiles }set
  ts tset-print
  l0 layer{ ts tset 0 tidx }bitmap-mode
  l0 layer-print
  ts pxl{ 0 tidx 0 0 vec2 xy #255 color }set
  ." pxl 0,0 : " ts pxl{ 0 0 vec2 xy }get . cr
  ." pxl 1,0 : " ts pxl{ 1 0 vec2 xy }get . cr

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

ts tset-deinit

