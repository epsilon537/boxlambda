0 variable w
0 variable h
0 variable b
<tset> ts
<tmap> tm

: tileset-params

  l{ 8 , 16 , 32 , 64 }l
  [:
    dup w !
    h !
    l{ 1 , 2 , 4 , 8 }l
    [:
      b !
      ." width: " w @ . ."  height: " h @ . ." bpp: " b @ . cr
      ts tset{ w @ width h @ height b @ bpp 8 tiles }set
      ts tset-print
    ;] iter
  ;] iter
;

[: tileset-params ;] &>file tst_dir/vera-tileset-params.log

s" tst_dir/vera-tileset-params.log" s" vera-tileset-params.ref" f_cmp ?assert

ts tset-deinit

