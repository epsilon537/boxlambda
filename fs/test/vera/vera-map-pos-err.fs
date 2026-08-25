<tmap> tm
0 variable wh

false quit-on-xassert !

: map-pos-err-test

  l{ 64 , 128 , 256 }l
  [:
    wh !
    tm tmap{ wh @ width wh @ height TMAP-TXT16 type }apply
    tm tmap-print
    tm mapentry{ wh @ 0 vec2 xy BLUE bg WHITE fg 1 tidx }apply
    tm mapentry{ 0 wh @ vec2 xy BLUE bg WHITE fg 1 tidx }apply
  ;] iter
;

[: map-pos-err-test ;] &>file tst_dir/vera-map-pos-err.log

s" tst_dir/vera-map-pos-err.log" s" vera-map-pos-err.ref" f_cmp ?assert

true quit-on-xassert !
tm tmap-deinit

