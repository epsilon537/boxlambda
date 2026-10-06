<sheet-bitmap> bs0
<sheet-bitmap> bs1
<sheet-tileset> ts2

<tmap> tm0
<tmap> tm1

1 <spr> spr1
2 <spr> spr2

: stack-params-test
  bs0 sheet{ 320 width 32 height 4 bpp }apply
  bs0 sheet.
  vram-reset
  4 32 320 bs1 sheet{ width height bpp }set
  bs1 sheet-params-apply
  bs1 sheet.

  tm0 tmap{ 32 width 64 height 2 type }apply
  tm0 tmap.
  vram-reset
  2 64 32 tm1 tmap{ width height type }set
  tm1 tmap-params-apply
  tm1 tmap.

  tm1 mapentry{ 3 2 vec2 xy 1 flip 2 pal-group 2 tidx }apply
  3 2 vec2 tm1 mapentry@ hex. cr
  tm1 mapentry{ 3 2 vec2 xy 0 flip 0 pal-group 0 tidx }apply
  2 2 1 3 2 vec2 tm1 mapentry{ xy flip pal-group tidx }set
  tm1 mapentry-params-apply
  3 2 vec2 tm1 mapentry@ hex. cr

  bs1 pxl{ 1 2 vec2 xy CYAN color }apply
  bs1 pxl{ 1 2 vec2 xy }get hex. cr
  bs1 pxl{ 1 2 vec2 xy BLACK color }apply
  CYAN 1 2 vec2 bs1 pxl{ xy color }set
  bs1 pxl-params-apply
  1 2 vec2 bs1 pxl{ xy }get hex. cr

  ts2 sheet{ #32 width #32 height 8 bpp 8 tiles }apply
  spr1 spr{ ts2 sheet 1 tidx SPR-L0-L1 z 1 pal-group 5 6 vec2 xy 1 colmask HFLIP flip }apply
  spr1 spr.
  HFLIP 1 5 6 vec2 1 SPR-L0-L1 1 ts2 spr2 spr{ sheet tidx z pal-group xy colmask flip }set
  spr2 spr-params-apply
  spr2 spr.
;

[: stack-params-test ;] &>file tst_dir/vera-stack-params.log

s" tst_dir/vera-stack-params.log" s" vera-stack-params.ref" f_cmp ?assert

bs0 sheet-deinit
bs1 sheet-deinit
ts2 sheet-deinit
tm1 tmap-deinit
spr1 spr-deinit
spr2 spr-deinit

