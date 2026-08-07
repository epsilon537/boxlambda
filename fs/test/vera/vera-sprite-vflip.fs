
<tset> ts
1 <spr> spr

: sprite-vflip

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ #64 width #64 height 8 bpp 8 tiles }set
  ts tset-print
  spr spr{ ts tset 2 tidx SPR-L1 z 0 paloffset 0 0 vec2 xy 1 colmask VFLIP flip }set
  
  spr spr-flip@ . cr

  64 0 do
    64 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }set
    loop
  loop

  #63 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y63 capture: $" #0 line-capture-pxl@ hex. cr
  ." x1 y63 capture: $" #1 line-capture-pxl@ hex. cr

  #0 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr
  ." x7 y0 capture: $" #7 line-capture-pxl@ hex. cr
  ." x63 y0 capture: $" #63 line-capture-pxl@ hex. cr
  ." x64 y0 capture: $" #64 line-capture-pxl@ hex. cr

  #64 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y64 capture: $" #0 line-capture-pxl@ hex. cr
;

[: sprite-vflip ;] &>file tst_dir/vera-sprite-vflip.log

s" tst_dir/vera-sprite-vflip.log" s" vera-sprite-vflip.ref" f_cmp ?assert

ts tset-deinit

