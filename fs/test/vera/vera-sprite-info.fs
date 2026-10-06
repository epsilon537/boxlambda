
<sheet-tileset> ts
1 <spr> spr

: sprite-info-test

  true sprites-enable

  ts sheet{ #32 width #32 height 8 bpp 8 tiles }apply
  ts sheet.
  spr spr{ ts sheet 1 tidx SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply
  
  spr spr.
;

[: sprite-info-test ;] &>file tst_dir/vera-sprite-info.log

s" tst_dir/vera-sprite-info.log" s" vera-sprite-info.ref" f_cmp ?assert

ts sheet-deinit
spr spr-deinit

