<tset> ts

: bitmap-pos-err-test

  [: ts tset{ 320 width 64 height 1 bpp 1 tiles }apply ;] try ?dup if execute then
  [: ts pxl{ 0 tidx 320 0 vec2 xy #101 color }apply ;] try ?dup if execute then
  [: ts pxl{ 0 tidx 0 64 vec2 xy #101 color }apply ;] try ?dup if execute then

  [: ts tset{ 640 width 32 height 1 bpp 1 tiles }apply ;] try ?dup if execute then
  [: ts pxl{ 0 tidx 640 0 vec2 xy #101 color }apply ;] try ?dup if execute then
  [: ts pxl{ 0 tidx 0 32 vec2 xy #101 color }apply ;] try ?dup if execute then
;

[: bitmap-pos-err-test ;] &>file tst_dir/vera-bitmap-pos-err.log

s" tst_dir/vera-bitmap-pos-err.log" s" vera-bitmap-pos-err.ref" f_cmp ?assert

ts tset-deinit

