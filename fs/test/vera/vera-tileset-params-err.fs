<tset> ts

false quit-on-xassert !

: tileset-params-err

  ." 1:" cr
  ts tset{ #33 width #32 height 8 bpp 8 tiles }apply
  ." 2:" cr
  ts tset{ #32 width #33 height 8 bpp 8 tiles }apply
  ." 3:" cr
  ts tset{ #32 width #32 height 9 bpp 8 tiles }apply
  ." 4:" cr
  ts tset{ #32 width #32 height 8 bpp #1024 tiles }apply
  ." 5:" cr
  ts tset{ #32 width #32 height 8 bpp #1023 tiles }apply
;

[: tileset-params-err ;] &>file tst_dir/vera-tileset-params-err.log

s" tst_dir/vera-tileset-params-err.log" s" vera-tileset-params-err.ref" f_cmp ?assert

ts tset-deinit
true quit-on-xassert !

