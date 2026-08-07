
<tset> ts
0 <spr> spr0
63 <spr> spr63

: sprite-first-last

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ 8 width 8 height 8 bpp 8 tiles }set
  ts tset-print
  spr0 spr{ ts tset 2 tidx SPR-L1 z 0 paloffset #40 #50 vec2 xy 1 colmask }set
  spr63 spr{ ts tset 2 tidx SPR-L1 z 0 paloffset #40 #70 vec2 xy 1 colmask }set

  8 0 do
    8 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }set
    loop
  loop

  #50 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x39 y50 capture: $" #39 line-capture-pxl@ hex. cr
  ." x40 y50 capture: $" #40 line-capture-pxl@ hex. cr

  #70 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x39 y70 capture: $" #39 line-capture-pxl@ hex. cr
  ." x40 y50 capture: $" #40 line-capture-pxl@ hex. cr
;

[: sprite-first-last ;] &>file tst_dir/vera-sprite-first-last.log

s" tst_dir/vera-sprite-first-last.log" s" vera-sprite-first-last.ref" f_cmp ?assert

ts tset-deinit

