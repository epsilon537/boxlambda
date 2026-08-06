<tset> ts
<tmap> tm

: vera-scroll-test

  true display-enable

  ts tset{ #16 width #16 height 8 bpp #32 tiles }set
  ts tset-print
  tm tmap{ #32 width #32 height TMAP-TILE type }set
  tm tmap-print
  l0 layer{ ts tset tm tmap }tilemap-mode
  true l0 layer-enable
  false l1 layer-enable
  false sprites-enable

  tm mapentry{ 2 2 vec2 xy 0 flip 0 paloffset 1 tidx }set

  #16 0 do
    ts pxl{ 1 tidx i i vec2 xy WHITE color }set
    ts pxl{ 1 tidx #15 i - i vec2 xy WHITE color }set
  loop

  #16 l0 layer-hscroll!
  #16 l0 layer-vscroll!
  ." hscroll: " l0 layer-hscroll@ . cr
  ." vscroll: " l0 layer-vscroll@ . cr

  #16 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 15: " #15 line-capture-pxl@ hex. cr
  ." capture 16: " #16 line-capture-pxl@ hex. cr
  ." capture 17: " #17 line-capture-pxl@ hex. cr

  #32 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 31: " #31 line-capture-pxl@ hex. cr
  ." capture 32: " #32 line-capture-pxl@ hex. cr
  ." capture 33: " #33 line-capture-pxl@ hex. cr
;

[: vera-scroll-test ;] &>file tst_dir/vera-scroll.log

s" tst_dir/vera-scroll.log" s" vera-scroll.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

