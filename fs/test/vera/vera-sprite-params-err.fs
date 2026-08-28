
<tset> ts
1 <spr> spr

: sprite-params-err

  true sprites-enable

  ts tset{ #32 width #32 height 8 bpp 8 tiles }apply
  ts tset-print
  ." 1: " cr
  [: spr spr{ 1 tidx 0 tset SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 2: " cr
  [: spr spr{ 8 tidx ts tset SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 3: " cr
  [: spr spr{ 7 tidx ts tset 9 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 4: " cr
  [: spr spr{ 7 tidx ts tset SPR-L0-L1 z 1 paloffset #1024 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 5: " cr
  [: spr spr{ 7 tidx ts tset SPR-L0-L1 z 1 paloffset 5 #1024 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
  ." 6: " cr
  [: spr spr{ 7 tidx ts tset SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask #10 flip }apply ;] try ?dup if execute then
  ." 7: " cr
  [: spr spr{ 7 tidx ts tset SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply ;] try ?dup if execute then
 
  spr spr-print
;

[: sprite-params-err ;] &>file tst_dir/vera-sprite-params-err.log

s" tst_dir/vera-sprite-params-err.log" s" vera-sprite-params-err.ref" f_cmp ?assert

ts tset-deinit

