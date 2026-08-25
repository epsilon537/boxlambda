#10 constant row1
#20 constant col1
#20 constant row2
#10 constant col2
#16 constant row3
#16 constant col3

create rows row1 , row2 , row3 ,
create cols col1 , col2 , col3 ,

0 variable row
0 variable col

<tset> ts
<tmap> tm

: mapentry-tile

  true display-enable
  false l0 layer-enable
  false l1 layer-enable

  ts tset{ #16 width #16 height 8 bpp #32 tiles }apply
  ts tset-print

  16 0 do
    i 1+ 0 ?do
      ts pxl{ 1 tidx i j vec2 xy BLUE color }apply
    loop
  loop

  tm tmap{ #32 width #32 height TMAP-TILE type }apply
  tm tmap-print
  l0 layer{ ts tset tm tmap }tilemap-mode
  l0 layer-print

  tm mapentry{ col1 row1 vec2 xy 0 flip 0 paloffset 1 tidx }apply
  tm mapentry{ col2 row2 vec2 xy VFLIP_HFLIP flip 0 paloffset 1 tidx }apply
  tm mapentry{ col3 row3 vec2 xy 0 flip 1 paloffset 1 tidx }apply

  true l0 layer-enable
  false l1 layer-enable

  3 0 do
    i cells rows + @ row !
    i cells cols + @ col !
    row @ #16 * irqline!
    true line-capture-enable
    begin line-capture-enabled? not until
    #16 0 do
      ." capture: " i . ." : " col @ #16 * i + line-capture-pxl@ hex. cr
    loop
  loop
;

[: mapentry-tile ;] &>file tst_dir/vera-mapentry-tile.log

s" tst_dir/vera-mapentry-tile.log" s" vera-mapentry-tile.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

