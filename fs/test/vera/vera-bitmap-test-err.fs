<tset> ts

false quit-on-xassert !

: bitmap-test-err

  ts tset{ 321 width 64 height 1 bpp 1 tiles }apply
  ts tset-print
  ts tset{ 320 width 0 height 1 bpp 1 tiles }apply
  ts tset-print
  ts tset{ 320 width 64 height 0 bpp 1 tiles }apply
  ts tset-print
  ts tset{ 320 width 64 height 1 bpp 0 tiles }apply
  ts tset-print
  [: ts tset{ 640 width 4095 height 8 bpp 1 tiles }apply ;] try code>link link>name ctype cr
  ts tset-print
  ts tset{ 320 width 4096 height 1 bpp 1 tiles }apply
  ts tset-print
;

[: bitmap-test-err ;] &>file tst_dir/vera-bitmap-test-err.log

s" tst_dir/vera-bitmap-test-err.log" s" vera-bitmap-test-err.ref" f_cmp ?assert

true quit-on-xassert !
ts tset-deinit

