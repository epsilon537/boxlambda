0 variable wh
0 variable mt

<tmap> tm

: map-test

  
  l{ 32 , 64 , 128 }l
  [:
    wh !

    l{ 0 , 1 , 2 }l
    [:
      mt !
      tm tmap{ wh @ width wh @ height mt @ type }set
      tm tmap-print
    ;] iter
  ;] iter
;

[: map-test ;] &>file tst_dir/vera-map-test.log

s" tst_dir/vera-map-test.log" s" vera-map-test.ref" f_cmp ?assert

tm tmap-deinit

