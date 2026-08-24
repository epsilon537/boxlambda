<tset> ts

: bitmap-tilesize-test
  ts tset{ 320 width 32 height 1 bpp 2 tiles }set
  ." Tile size :" ts tset-tilesize@ hex. cr
  ." Tile 0: " 0 ts tset-tidx>addr hex. cr
  ." Tile 1: " 1 ts tset-tidx>addr hex. cr
;

[: bitmap-tilesize-test ;] &>file tst_dir/vera-bitmap-tilesize.log

s" tst_dir/vera-bitmap-tilesize.log" s" vera-bitmap-tilesize.ref" f_cmp ?assert

ts tset-deinit
