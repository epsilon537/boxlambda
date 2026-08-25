create widths 32 , 256 ,
create heights 256 , 32 ,

0 variable w
0 variable h
0 variable t

<tmap> tm

: map-test-cont

  2 0 do
    i cells widths + @ w !
    i cells heights + @ h !
    tm tmap{ w @ width h @ height t @ type }apply
    tm tmap-print
    1 t +!
  loop
;

[: map-test-cont ;] &>file tst_dir/vera-map-test-cont.log

s" tst_dir/vera-map-test-cont.log" s" vera-map-test-cont.ref" f_cmp ?assert

tm tmap-deinit

