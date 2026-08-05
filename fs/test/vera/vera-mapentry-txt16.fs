#10 constant row1
#20 constant col1
#20 constant row2
#10 constant col2

create rows row1 , row2 ,
create cols col1 , col2 ,

0 variable row
0 variable col

<tset> ts
<tmap> tm

: mapentry-txt16

  true display-enable

  ts tset{ #16 width #16 height 1 bpp #32 tiles }set
  ts tset-print

  16 0 do
    ts pxl{ 1 tidx i i vec2 xy WHITE color }set
    ts pxl{ 1 tidx 15 i - i vec2 xy WHITE color }set
  loop

  tm tmap{ #32 width #32 height TMAP-TXT16 type }set
  tm tmap-print
  l0 layer{ ts tset tm tmap }tilemap-mode
  l0 layer-print

  tm mapentry{ col1 row1 vec2 xy GREEN bg WHITE fg 1 tidx }set
  tm mapentry{ col2 row2 vec2 xy BLUE bg YELLOW fg 1 tidx }set

  true l0 layer-enable
  false l1 layer-enable

  2 0 do
    i cells rows + @ row !
    i cells cols + @ col !
    row @ #16 * irqline-set
    true line-capture-enable
    begin line-capture-enabled? not until
    #16 0 do
      ." capture: " i . ." : " col @ #16 * i + line-capture-pxl@ hex. cr
    loop
  loop
;

[: mapentry-txt16 ;] &>file tst_dir/vera-mapentry-txt16.log

s" tst_dir/vera-mapentry-txt16.log" s" vera-mapentry-txt16.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

