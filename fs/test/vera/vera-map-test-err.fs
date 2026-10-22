<tmap> tm0
<tmap> tm1
0 variable wh

: map-test-err

  ." 1:" cr
  [: tm0 tmap{ 33 width 32 height TMAP-TXT16 type }apply ;] try ?dup if execute then
  ." 2:" cr
  [: tm0 tmap{ 32 width 33 height TMAP-TXT16 type }apply ;] try ?dup if execute then
  ." 3:" cr
  [: tm0 tmap{ 32 width 32 height 5 type }apply ;] try ?dup if execute then
  ." 4:" cr
  [: tm0 tmap{ 256 width 256 height 0 type }apply ;] try ?dup if execute then
  ." 5:" cr
  [: tm1 tmap{ 32 width 32 height 0 type }apply ;] try ?dup if execute then
;

[: map-test-err ;] &>file tst_dir/vera-map-test-err.log

s" tst_dir/vera-map-test-err.log" s" vera-map-test-err.ref" f_cmp ?assert

tm0 tmap-deinit
tm1 tmap-deinit

