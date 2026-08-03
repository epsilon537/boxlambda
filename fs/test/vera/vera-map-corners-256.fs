create cols 256 , 256 , 256 , 32  , 64  , 128 ,
create rows 32  , 64  , 128 , 256 , 256 , 256 ,
0 variable r
0 variable c

<tset> ts
<tmap> tm

: map-corners-256

  true display-enable

  ts tset{ 16 width 16 height 1 bpp 32 tiles }set
  ts tset-print

  16 0 do
    pxl{ ts tset 1 tidx i i vec2 xy 1 color }set
    pxl{ ts tset 1 tidx 15 i - i vec2 xy 1 color }set
  loop

  6 0 do
    i cells cols + @ c !
    i cells rows + @ r !
    tm tmap{ c @ width r @ height TMAP-TXT16 type }set
    tm tmap-print
    l0 layer{ ts tset tm tmap }tilemap-mode
    l0 layer-print
    mapentry{ tm tmap GREEN bg WHITE fg 1 tidx 0 0 vec2 xy }set
    mapentry{ tm tmap BLUE bg WHITE fg 1 tidx c @ 1- 0 vec2 xy }set
    mapentry{ tm tmap PURPLE bg YELLOW fg 1 tidx 0 r @ 1- vec2 xy }set
    mapentry{ tm tmap GREEN bg GREY fg 1 tidx c @ 1- r @ 1- vec2 xy }set
    ." 0 0: " 0 0 vec2 tm mapentry@ hex. cr
    ." c-1 0: " c @ 1- 0 vec2 tm mapentry@ hex. cr
    ." 0 r-1: " 0 r @ 1- vec2 tm mapentry@ hex. cr
    ." c-1 r-1: " c @ 1- r @ 1- vec2 tm mapentry@ hex. cr
  loop
;

[: map-corners-256 ;] &>file tst_dir/vera-map-corners-256.log

s" tst_dir/vera-map-corners-256.log" s" vera-map-corners-256.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

