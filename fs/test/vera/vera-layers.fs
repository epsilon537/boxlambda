<tset> ts
<tmap> tm0
<tmap> tm1

: vera-layers
  true display-enable
  false l0 layer-enable
  false l1 layer-enable
  ." l0 enabled? " l0 layer-enabled? . cr
  ." l1 enabled? " l1 layer-enabled? . cr
  ts tset{ #16 width #16 height 8 bpp #32 tiles }set
  ts tset-print
  tm0 tmap{ #32 width #32 height TMAP-TILE type }set
  tm1 tmap{ #32 width #32 height TMAP-TILE type }set
  tm0 tmap-print
  tm1 tmap-print
  l0 layer{ ts tset tm0 tmap }tilemap-mode
  l1 layer{ ts tset tm1 tmap }tilemap-mode
  tm0 mapentry{ 1 1 vec2 xy 1 tidx }set
  tm1 mapentry{ 1 1 vec2 xy 2 tidx }set

  #16 0 do
    ts pxl{ 1 tidx 0 i vec2 xy RED color }set
  loop

  #16 0 do
    ts pxl{ 2 tidx 0 i vec2 xy BLUE color }set
  loop
 
  #16 irqline!
  true line-capture-enable
  begin line-capture-enabled? not until
  ." 16, 16 capture: $" 16 line-capture-pxl@ hex. cr

  true l0 layer-enable
  ." l0 enabled? " l0 layer-enabled? . cr

  true line-capture-enable
  begin line-capture-enabled? not until
  ." 16, 16 capture: $" 16 line-capture-pxl@ hex. cr

  true l1 layer-enable
  ." l1 enabled? " l1 layer-enabled? . cr

  true line-capture-enable
  begin line-capture-enabled? not until
  ." 16, 16 capture: $" 16 line-capture-pxl@ hex. cr
;

[: vera-layers ;] &>file tst_dir/vera-layers.log

s" tst_dir/vera-layers.log" s" vera-layers.ref" f_cmp ?assert

tm0 tmap-deinit
tm1 tmap-deinit
ts tset-deinit

