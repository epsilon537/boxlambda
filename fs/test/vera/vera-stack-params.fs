<tset> ts0
<tset> ts1

<tmap> tm0
<tmap> tm1

: stack-params-test
  ts0 tset{ 320 width 32 height 4 bpp 8 tiles }apply
  ts0 tset-print
  vram :: reset
  8 4 32 320 ts1 tset{ width height bpp tiles }apply
  ts1 tset-print

  tm0 tmap{ 32 width 64 height 2 type }apply
  tm0 tmap-print
  vram :: reset
  2 64 32 tm1 tmap{ width height type }apply
  tm1 tmap-print

  tm1 mapentry{ 3 2 vec2 xy 1 flip 2 paloffset 2 tidx }apply
  3 2 vec2 tm1 mapentry@ hex. cr
  tm1 mapentry{ 3 2 vec2 xy 0 flip 0 paloffset 0 tidx }apply
  2 2 1 3 2 vec2 tm1 mapentry{ xy flip 2 paloffset tidx }apply
  3 2 vec2 tm1 mapentry@ hex. cr

  ts1 pxl{ 2 tidx 1 2 vec2 xy CYAN color }apply
  ts1 pxl{ 1 2 vec2 xy }get hex. cr
  ts1 pxl{ 2 tidx 1 2 vec2 xy BLACK color }apply
  CYAN 1 2 vec2 2 ts1 pxl{ tidx xy color }apply
  1 2 vec2 ts1 pxl{ xy }get hex. cr
;

[: stack-params-test ;] &>file tst_dir/vera-stack-params.log

s" tst_dir/vera-stack-params.log" s" vera-stack-params.ref" f_cmp ?assert

ts1 tset-deinit
tm1 tmap-deinit


