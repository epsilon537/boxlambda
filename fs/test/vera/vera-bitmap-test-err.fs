<sheet-bitmap> ts

: bitmap-test-err

  ." 1: " cr
  [: bitmap sheet{ 321 width 64 height 1 bpp }apply ;] try ?dup if execute then
  ." 2: " cr
  [: bitmap sheet{ 320 width 0 height 1 bpp }apply ;] try ?dup if execute then
  ." 3: " cr
  [: bitmap sheet{ 320 width 64 height 0 bpp }apply ;] try ?dup if execute then
  ." 4: " cr
  [: bitmap sheet{ 320 width 64 height 1 bpp }apply ;] try ?dup if execute then
  ." 5: " cr
  [: bitmap sheet{ 640 width 4095 height 8 bpp }apply ;] try ?dup if execute then
  ." 6: " cr
  [: bitmap sheet{ 320 width 4096 height 1 bpp }apply ;] try ?dup if execute then
;

[: bitmap-test-err ;] &>file tst_dir/vera-bitmap-test-err.log

s" tst_dir/vera-bitmap-test-err.log" s" vera-bitmap-test-err.ref" f_cmp ?assert

ts sheet-deinit

