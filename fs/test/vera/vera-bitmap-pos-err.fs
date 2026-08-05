<tset> ts

false quit-on-xassert !

: bitmap-pos-err-test

  ts tset{ 320 width 64 height 1 bpp 1 tiles }set
  ts pxl{ 0 tidx 320 0 vec2 xy #101 color }set
  ts pxl{ 0 tidx 0 64 vec2 xy #101 color }set

  ts tset{ 640 width 32 height 1 bpp 1 tiles }set
  ts pxl{ 0 tidx 640 0 vec2 xy #101 color }set
  ts pxl{ 0 tidx 0 32 vec2 xy #101 color }set
;

[: bitmap-pos-err-test ;] &>file tst_dir/vera-bitmap-pos-err.log

s" tst_dir/vera-bitmap-pos-err.log" s" vera-bitmap-pos-err.ref" f_cmp ?assert

true quit-on-xassert !
ts tset-deinit

