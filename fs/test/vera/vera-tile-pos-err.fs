<tset> ts0
<tset> ts1

: tile-pos-err

  ts0 tset{ 8 width 8 height 8 bpp 8 tiles }apply
  ts0 tset-print
  [: ts0 pxl{ 1 tidx 8 0 vec2 xy #1 color }apply ;] try ?dup if execute then
  [: ts0 pxl{ 1 tidx 0 8 vec2 xy #1 color }apply ;] try ?dup if execute then

  ts1 tset{ 16 width 16 height 8 bpp 8 tiles }apply
  ts1 tset-print
  [: ts1 pxl{ 1 tidx 16 0 vec2 xy #1 color }apply ;] try ?dup if execute then
  [: ts1 pxl{ 1 tidx 0 16 vec2 xy #1 color }apply ;] try ?dup if execute then
;

[: tile-pos-err ;] &>file tst_dir/vera-tile-pos-err.log

s" tst_dir/vera-tile-pos-err.log" s" vera-tile-pos-err.ref" f_cmp ?assert

ts0 tset-deinit
ts1 tset-deinit

