<tset> ts

: palette-sys-colors-test

  true l0 layer-enable
  true display-enable

  ts tset{ #320 width #32 height 8 bpp 1 tiles }apply
  ts tset-print
  l0 layer{ ts tset 0 tidx }bitmap-mode
  l0 layer-print

  ts pxl{ 0 tidx 0 0 vec2 xy BLACK color }apply
  ts pxl{ 0 tidx 1 0 vec2 xy WHITE color }apply
  ts pxl{ 0 tidx 2 0 vec2 xy RED color }apply
  ts pxl{ 0 tidx 3 0 vec2 xy CYAN color }apply
  ts pxl{ 0 tidx 4 0 vec2 xy PURPLE color }apply
  ts pxl{ 0 tidx 5 0 vec2 xy GREEN color }apply
  ts pxl{ 0 tidx 6 0 vec2 xy BLUE color }apply
  ts pxl{ 0 tidx 7 0 vec2 xy YELLOW color }apply
  ts pxl{ 0 tidx 8 0 vec2 xy ORANGE color }apply
  ts pxl{ 0 tidx 9 0 vec2 xy BROWN color }apply
  ts pxl{ 0 tidx #10 0 vec2 xy LIGHT-RED color }apply
  ts pxl{ 0 tidx #11 0 vec2 xy DARK-GREY color }apply
  ts pxl{ 0 tidx #12 0 vec2 xy GREY color }apply
  ts pxl{ 0 tidx #13 0 vec2 xy LIGHT-GREEN color }apply
  ts pxl{ 0 tidx #14 0 vec2 xy LIGHT-BLUE color }apply
  ts pxl{ 0 tidx #15 0 vec2 xy LIGHT-GREY color }apply

  #16 #0 do
    ts pxl{ 0 tidx i 16 + 0 vec2 xy i greyscale color }apply
  loop

  0 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until

  #32 #0 do
    i line-capture-pxl@ hex. cr
  loop

;

[: palette-sys-colors-test ;] &>file tst_dir/vera-palette-sys-colors.log

s" tst_dir/vera-palette-sys-colors.log" s" vera-palette-sys-colors.ref" f_cmp ?assert

ts tset-deinit

