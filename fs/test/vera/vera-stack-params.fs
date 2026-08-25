<tset> ts0
<tset> ts1
<tset> ts2

<tmap> tm0
<tmap> tm1

1 <spr> spr1
2 <spr> spr2

: stack-params-test
  ts0 tset{ 320 width 32 height 4 bpp 8 tiles }apply
  ts0 tset-print
  vram :: reset
  8 4 32 320 ts1 tset{ width height bpp tiles }set
  ts1 tset-params-apply
  ts1 tset-print

  tm0 tmap{ 32 width 64 height 2 type }apply
  tm0 tmap-print
  vram :: reset
  2 64 32 tm1 tmap{ width height type }set
  tm1 tmap-params-apply
  tm1 tmap-print

  tm1 mapentry{ 3 2 vec2 xy 1 flip 2 paloffset 2 tidx }apply
  3 2 vec2 tm1 mapentry@ hex. cr
  tm1 mapentry{ 3 2 vec2 xy 0 flip 0 paloffset 0 tidx }apply
  2 2 1 3 2 vec2 tm1 mapentry{ xy flip 2 paloffset tidx }set
  tm1 mapentry-params-apply
  3 2 vec2 tm1 mapentry@ hex. cr

  ts1 pxl{ 2 tidx 1 2 vec2 xy CYAN color }apply
  ts1 pxl{ 1 2 vec2 xy }get hex. cr
  ts1 pxl{ 2 tidx 1 2 vec2 xy BLACK color }apply
  CYAN 1 2 vec2 2 ts1 pxl{ tidx xy color }set
  ts1 pxl-params-apply
  1 2 vec2 ts1 pxl{ xy }get hex. cr

  ts2 tset{ #32 width #32 height 8 bpp 8 tiles }apply
  spr1 spr{ ts2 tset 1 tidx SPR-L0-L1 z 1 paloffset 5 6 vec2 xy 1 colmask HFLIP flip }apply
  spr1 spr-print
  HFLIP 1 5 6 vec2 1 SPR-L0-L1 1 ts2 spr2 spr{ tset tidx z paloffset xy colmask flip }set
  spr2 spr-params-apply
  spr2 spr-print
;

[: stack-params-test ;] &>file tst_dir/vera-stack-params.log

s" tst_dir/vera-stack-params.log" s" vera-stack-params.ref" f_cmp ?assert

ts1 tset-deinit
ts2 tset-deinit
tm1 tmap-deinit


