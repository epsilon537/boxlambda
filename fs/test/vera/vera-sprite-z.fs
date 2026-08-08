
<tset> ts
<tmap> tm0
<tmap> tm1
1 <spr> spr

: sprite-z

  true display-enable
  true sprites-enable

  tm0 tmap{ 32 width 32 height TMAP-TILE type }set
  tm1 tmap{ 32 width 32 height TMAP-TILE type }set
  ts tset{ #8 width #8 height 8 bpp 8 tiles }set
  ts tset-print
  l0 layer{ ts tset tm0 tmap }tilemap-mode
  l1 layer{ ts tset tm1 tmap }tilemap-mode
  tm0 mapentry{ 0 0 vec2 xy 0 paloffset 2 tidx }set
  tm1 mapentry{ 0 0 vec2 xy 2 paloffset 2 tidx }set

  spr spr{ ts tset 2 tidx SPR-DIS z 1 paloffset 0 0 vec2 xy }set


  false l0 layer-enable
  false l1 layer-enable

  spr spr-xy@ .vec2 cr

  8 0 do
    8 i 1+ min 0 ?do
      ts pxl{ 2 tidx i j vec2 xy CYAN color }set
    loop
  loop

  ." Sprite disabled: " cr
  spr spr-z@ . cr
  #0 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr

  ." Sprite behind l0: " cr
  spr spr{ SPR-BG-L0 z }set
  spr spr-z@ . cr
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr


  ." l0 enable: " cr
  true l0 layer-enable
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr

  ." Sprite between l0 and l1: " cr
  spr spr{ SPR-L0-L1 z }set
  spr spr-z@ . cr
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr

  ." l1 enable: " cr
  true l1 layer-enable
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr

  ." sprite in front of l1: " cr
  spr spr{ SPR-L1 z }set
  spr spr-z@ . cr
  true line-capture-enable
  begin line-capture-enabled? not until
  ." x0 y0 capture: $" #0 line-capture-pxl@ hex. cr
;

[: sprite-z ;] &>file tst_dir/vera-sprite-z.log

s" tst_dir/vera-sprite-z.log" s" vera-sprite-z.ref" f_cmp ?assert

ts tset-deinit

