
<tset> ts
1 <spr> spr

false quit-on-xassert !

: sprite-params-err

  true sprites-enable

  ts tset{ #32 width #32 height 8 bpp 8 tiles }apply
  ts tset-print
  ." 1: " cr
  spr spr{ 0 tset 1 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
  ." 2: " cr
  spr spr{ ts tset 8 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
  ." 3: " cr
  spr spr{ ts tset 7 tidx 9 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
  ." 4: " cr
  spr spr{ ts tset 7 tidx SPR-L0-L1 z 1 paloffset #1024 6 vec2 xy 1 colmask HFLIP flip }apply
  ." 5: " cr
  spr spr{ ts tset 7 tidx SPR-L0-L1 z 1 paloffset 5 #1024 vec2 xy 1 colmask HFLIP flip }apply
  ." 6: " cr
  spr spr{ ts tset 7 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask #10 flip }apply
  ." 7: " cr
  spr spr{ ts tset 7 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
 
  spr spr-print
;

[: sprite-params-err ;] &>file tst_dir/vera-sprite-params-err.log

s" tst_dir/vera-sprite-params-err.log" s" vera-sprite-params-err.ref" f_cmp ?assert

ts tset-deinit
true quit-on-xassert !

