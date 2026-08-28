<tset> ts

: bitmap-test-err

  ." 1: " cr
  [: ts tset{ 321 width 64 height 1 bpp 1 tiles }apply ;] try ?dup if execute then
  ." 2: " cr
  [: ts tset{ 320 width 0 height 1 bpp 1 tiles }apply ;] try ?dup if execute then
  ." 3: " cr
  [: ts tset{ 320 width 64 height 0 bpp 1 tiles }apply ;] try ?dup if execute then
  ." 4: " cr
  [: ts tset{ 320 width 64 height 1 bpp 0 tiles }apply ;] try ?dup if execute then
  ." 5: " cr
  [: ts tset{ 640 width 4095 height 8 bpp 1 tiles }apply ;] try ?dup if execute then
  ." 6: " cr
  [: ts tset{ 320 width 4096 height 1 bpp 1 tiles }apply ;] try ?dup if execute then
;

[: bitmap-test-err ;] &>file tst_dir/vera-bitmap-test-err.log

s" tst_dir/vera-bitmap-test-err.log" s" vera-bitmap-test-err.ref" f_cmp ?assert

ts tset-deinit

