
<tset> ts
1 <spr> spr

: sprite-pal-group

  true display-enable
  true sprites-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ #64 width #64 height 8 bpp 4 tiles }apply
  ts tset.
  spr spr{ ts tset 2 tidx SPR-L1 z 0 pal-group 0 0 vec2 xy }apply

  64 0 do
    64 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }apply
    loop
  loop

  0 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr

  spr spr{ 1 pal-group }apply

  spr spr-pal-group@ . cr

  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr
;

[: sprite-pal-group ;] &>file tst_dir/vera-sprite-pal-group.log

s" tst_dir/vera-sprite-pal-group.log" s" vera-sprite-pal-group.ref" f_cmp ?assert

ts tset-deinit
spr spr-deinit

