<tset> ts

: tileset-params-err

  ." 1:" cr
  [: ts tset{ #33 width #32 height 8 bpp 8 tiles }apply ;] try ?dup if execute then
  ." 2:" cr
  [: ts tset{ #32 width #33 height 8 bpp 8 tiles }apply ;] try ?dup if execute then
  ." 3:" cr
  [: ts tset{ #32 width #32 height 9 bpp 8 tiles }apply ;] try ?dup if execute then
  ." 4:" cr
  [: ts tset{ #32 width #32 height 8 bpp #1024 tiles }apply ;] try ?dup if execute then
  ." 5:" cr
  [: ts tset{ #32 width #32 height 8 bpp #1023 tiles }apply ;] try ?dup if execute then
;

[: tileset-params-err ;] &>file tst_dir/vera-tileset-params-err.log

s" tst_dir/vera-tileset-params-err.log" s" vera-tileset-params-err.ref" f_cmp ?assert

ts tset-deinit

