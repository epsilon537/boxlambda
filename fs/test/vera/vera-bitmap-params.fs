0 variable w
0 variable h
0 variable b
<sheet-bitmap> bitmap

: tileset-params

  l{ 320 , 640 }l
  [:
    dup w !
    h !
    l{ 1 , 2 , 4 , 8 }l
    [:
      b !
      ." width: " w @ . ."  height: " h @ . ." bpp: " b @ . cr
      bitmap sheet{ w @ width h @ height b @ bpp }apply
      bitmap sheet.
    ;] iter
  ;] iter
;

[: bitmap-params ;] &>file tst_dir/vera-bitmap-params.log

s" tst_dir/vera-bitmap-params.log" s" vera-bitmap-params.ref" f_cmp ?assert

ts sheet-deinit

