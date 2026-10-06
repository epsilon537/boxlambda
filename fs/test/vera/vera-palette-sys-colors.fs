<sheet-bitmap> bitmap

: palette-sys-colors-test

  true l0 layer-enable
  true display-enable

  bitmap sheet{ #320 width #32 height 8 bpp }apply
  bitmap sheet.
  bitmap l0 layer-bitmap-mode
  l0 layer.

  bitmap pxl{ 0 0 vec2 xy BLACK color }apply
  bitmap pxl{ 1 0 vec2 xy WHITE color }apply
  bitmap pxl{ 2 0 vec2 xy RED color }apply
  bitmap pxl{ 3 0 vec2 xy CYAN color }apply
  bitmap pxl{ 4 0 vec2 xy PURPLE color }apply
  bitmap pxl{ 5 0 vec2 xy GREEN color }apply
  bitmap pxl{ 6 0 vec2 xy BLUE color }apply
  bitmap pxl{ 7 0 vec2 xy YELLOW color }apply
  bitmap pxl{ 8 0 vec2 xy ORANGE color }apply
  bitmap pxl{ 9 0 vec2 xy BROWN color }apply
  bitmap pxl{ #10 0 vec2 xy LIGHT-RED color }apply
  bitmap pxl{ #11 0 vec2 xy DARK-GREY color }apply
  bitmap pxl{ #12 0 vec2 xy GREY color }apply
  bitmap pxl{ #13 0 vec2 xy LIGHT-GREEN color }apply
  bitmap pxl{ #14 0 vec2 xy LIGHT-BLUE color }apply
  bitmap pxl{ #15 0 vec2 xy LIGHT-GREY color }apply

  #16 #0 do
    bitmap pxl{ i 16 + 0 vec2 xy i greyscale color }apply
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

bitmap sheet-deinit

