0 variable r
0 variable c

<tset> ts
<tmap> tm

: map-corners

  true display-enable

  ts tset{ 16 width 16 height 1 bpp 32 tiles }set
  ts tset-print

  16 0 do
    ts pxl{ 1 tidx i i vec2 xy 1 color }set
    ts pxl{ 1 tidx 15 i - i vec2 xy 1 color }set
  loop

  l{ 32 , 64 , 128 }l
  [:
    c !
    l{ 32 , 64 , 128 }l
    [:
      r !
      tm tmap{ c @ width r @ height TMAP-TXT16 type }set
      tm tmap-print
      l0 layer{ ts tset tm tmap }tilemap-mode
      true l0 layer-enable
      false l1 layer-enable
      l0 layer-print
      tm mapentry{ GREEN bg WHITE fg 1 tidx 0 0 vec2 xy }set
      tm mapentry{ BLUE bg WHITE fg 1 tidx c @ 1- 0 vec2 xy }set
      tm mapentry{ PURPLE bg YELLOW fg 1 tidx 0 r @ 1- vec2 xy }set
      tm mapentry{ GREEN bg GREY fg 1 tidx c @ 1- r @ 1- vec2 xy }set
      ." 0 0: " 0 0 vec2 tm mapentry@ hex. cr
      ." c-1 0: " c @ 1- 0 vec2 tm mapentry@ hex. cr
      ." 0 r-1: " 0 r @ 1- vec2 tm mapentry@ hex. cr
      ." c-1 r-1: " c @ 1- r @ 1- vec2 tm mapentry@ hex. cr
    ;] iter
  ;] iter
;

[: map-corners ;] &>file tst_dir/vera-map-corners.log

s" tst_dir/vera-map-corners.log" s" vera-map-corners.ref" f_cmp ?assert

ts tset-deinit
tm tmap-deinit

