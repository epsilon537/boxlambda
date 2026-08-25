
<tset> ts
1 <spr> spr

: sprite-xy

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ #64 width #64 height 8 bpp 8 tiles }apply
  ts tset-print
  spr spr{ ts tset 2 tidx SPR-L1 z #40 #50 vec2 xy }apply

  spr spr-xy@ .vec2 cr

  64 0 do
    64 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }apply
    loop
  loop

  #50 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x40 y50 capture: $" #40 line-capture-pxl@ hex. cr
  ." x41 y50 capture: $" #41 line-capture-pxl@ hex. cr

  [ #50 #63 + ] literal irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x40 yh-1 capture: $" #40 line-capture-pxl@ hex. cr
  ." x41 yh-1 capture: $" #41 line-capture-pxl@ hex. cr
  ." x40+w-1 yh-1 capture: $" [ #40 #63 + ] literal  line-capture-pxl@ hex. cr
  ." x40+w yh-1 capture: $" [ #40 #64 + ] literal line-capture-pxl@ hex. cr

  #50 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x40 yh capture: $" #40 line-capture-pxl@ hex. cr
  ." x47 yh capture: $" #47 line-capture-pxl@ hex. cr
;

[: sprite-xy ;] &>file tst_dir/vera-sprite-xy.log

s" tst_dir/vera-sprite-xy.log" s" vera-sprite-xy.ref" f_cmp ?assert

ts tset-deinit

