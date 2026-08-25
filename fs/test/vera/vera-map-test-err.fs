<tmap> tm0
<tmap> tm1
0 variable wh

false quit-on-xassert !

: map-test-err

  tm0 tmap{ 33 width 32 height TMAP-TXT16 type }apply
  tm0 tmap{ 32 width 33 height TMAP-TXT16 type }apply
  tm0 tmap{ 32 width 32 height 5 type }apply
  tm0 tmap{ 256 width 256 height 0 type }apply
  tm1 tmap{ 32 width 32 height 0 type }apply
;

[: map-test-err ;] &>file tst_dir/vera-map-test-err.log

s" tst_dir/vera-map-test-err.log" s" vera-map-test-err.ref" f_cmp ?assert

true quit-on-xassert !
tm0 tmap-deinit
tm1 tmap-deinit

