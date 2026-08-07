
<tset> ts
1 <spr> spr1
#65 <spr> spr65

: sprite-bank

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ 8 width 8 height 8 bpp 8 tiles }set
  ts tset-print
  spr1 spr{ ts tset 1 tidx SPR-L1 z 0 paloffset #40 #50 vec2 xy }set
  spr65 spr{ ts tset 1 tidx SPR-L1 z 1 paloffset #100 #200 vec2 xy }set
  ts pxl{ 1 tidx 0 0 vec2 xy CYAN color }set

  #50 irqline!
  0 sprite-bank!
  ." sprite-bank: " sprite-bank@ . cr

  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 40, 50: $" #40 line-capture-pxl@ hex. cr

  #200 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 100, 200 $" #100 line-capture-pxl@ hex. cr

  #50 irqline!
  1 sprite-bank!
  ." sprite-bank: " sprite-bank@ . cr

  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 40, 50: $" #40 line-capture-pxl@ hex. cr

  #200 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." capture 100, 200 $" #100 line-capture-pxl@ hex. cr
;

[: sprite-bank ;] &>file tst_dir/vera-sprite-bank.log

s" tst_dir/vera-sprite-bank.log" s" vera-sprite-bank.ref" f_cmp ?assert

ts tset-deinit

