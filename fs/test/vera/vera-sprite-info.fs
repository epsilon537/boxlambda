
<tset> ts
1 <spr> spr

: sprite-info-test

  true sprites-enable

  ts tset{ #32 width #32 height 8 bpp 8 tiles }apply
  ts tset-print
  spr spr{ ts tset 1 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
  
  spr spr-print
;

[: sprite-info-test ;] &>file tst_dir/vera-sprite-info.log

s" tst_dir/vera-sprite-info.log" s" vera-sprite-info.ref" f_cmp ?assert

ts tset-deinit

