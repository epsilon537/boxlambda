
<sheet-tileset> ts
1 <spr> spr

: sprite-params-err

  true sprites-enable

  ts sheet{ #32 width #32 height 8 bpp 8 tiles }apply
  ts sheet.
  ." 1: " cr
  [: spr spr{ 1 tidx 0 sheet SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 2: " cr
  [: spr spr{ 8 tidx ts sheet SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 3: " cr
  [: spr spr{ 7 tidx ts sheet 9 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 4: " cr
  [: spr spr{ 7 tidx ts sheet SPR-L0-L1 z 1 pal-group #1024 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 5: " cr
  [: spr spr{ 7 tidx ts sheet SPR-L0-L1 z 1 pal-group 5 #1024 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 6: " cr
  [: spr spr{ 7 tidx ts sheet SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask #10 flip }apply ;] try ?dup if execute then
  ." 7: " cr
  [: spr spr{ 7 tidx ts sheet SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
 
  spr spr.
;

[: sprite-params-err ;] &>file tst_dir/vera-sprite-params-err.log

s" tst_dir/vera-sprite-params-err.log" s" vera-sprite-params-err.ref" f_cmp ?assert

ts sheet-deinit
spr spr-deinit

