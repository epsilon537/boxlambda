<tmap> tm
0 variable wh

: map-pos-err-test

  l{ 64 , 128 , 256 }l
  [:
    wh !
    tm tmap{ wh @ width wh @ height TMAP-TXT16 type }apply
    tm tmap-print
    ." 1: " cr
    [: tm mapentry{ wh @ 0 vec2 xy BLUE bg WHITE fg 1 tidx }apply ;] try ?dup if execute then
    ." 2: " cr
    [: tm mapentry{ 0 wh @ vec2 xy BLUE bg WHITE fg 1 tidx }apply ;] try ?dup if execute then
  ;] iter
;

[: map-pos-err-test ;] &>file tst_dir/vera-map-pos-err.log

s" tst_dir/vera-map-pos-err.log" s" vera-map-pos-err.ref" f_cmp ?assert

tm tmap-deinit

