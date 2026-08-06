<tset> ts
<tmap> tm

: vera-scale-test

  true display-enable

  tm tmap{ #32 width #32 height TMAP-TILE type }set
  tm tmap-print
  ts tset{ #16 width #16 height 8 bpp #32 tiles }set
  ts tset-print
  l0 layer{ ts tset tm tmap }tilemap-mode
  true l0 layer-enable
  false l1 layer-enable

  tm mapentry{ 0 0 vec2 xy 0 flip 0 paloffset 1 tidx }set

  #16 0 do
    ts pxl{ 1 tidx i i vec2 xy WHITE color }set
    ts pxl{ 1 tidx #15 i - i vec2 xy WHITE color }set
  loop

  #15 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 14: " #14 line-capture-pxl@ hex. cr
  ." capture 15: " #15 line-capture-pxl@ hex. cr
  ." capture 16: " #16 line-capture-pxl@ hex. cr

  $40 hscale!
  $40 vscale!
  ." hscale: " hscale@ . cr
  ." vscale: " vscale@ . cr

  #31 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 29: " #29 line-capture-pxl@ hex. cr
  ." capture 30: " #30 line-capture-pxl@ hex. cr
  ." capture 31: " #31 line-capture-pxl@ hex. cr
  ." capture 32: " #32 line-capture-pxl@ hex. cr
;

[: vera-scale-test ;] &>file tst_dir/vera-scale.log

s" tst_dir/vera-scale.log" s" vera-scale.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

