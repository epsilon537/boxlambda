: x-process-font-dot-star-err ." Font file ./* error" cr ;

320 constant XRES
240 constant YRES
XRES 8 / constant #COLS
YRES 8 / constant #ROWS

begin-structure process-glyph-struct
  field: .glyph-base
  field: .font-fp
  field: .fontline
  field: .fontlinelen
end-structure

create (pgs) process-glyph-struct allot

<tset> ts
<tmap> tm

( addr -- c )
: s>c
  [ 1 1 stack-checker ]
  0 swap ( c addr )
  8 0 do
    dup i + c@ [char] * = if ( c addr )
      swap 8 i - 1<< or swap ( c addr )
    else
      dup i + c@ [char] . <> triggers x-process-font-dot-star-err ( c addr )
    then
  loop
  drop ( c )
;

\ True means a complete fontline has been read
( -- f )
: read-font-line
  [ 0 1 stack-checker ]
  begin
    (pgs) .font-fp @ (pgs) .fontline @ 256 f_gets ( addr len )
    2dup type cr
    nip ( len )
    dup (pgs) .fontlinelen ! ( len )
    0= if true
    else
      (pgs) .fontline @ c@ ( char )
      dup [char] # <> ( char f )
      over #13 <> ( char f f )
      rot #10 <> ( f f f )
      and and ( f )
    then
  until
  (pgs) .fontlinelen @ 8 >= ( f )
;

\ May raise x-process-font-err.
\ True means a complete glyph has been processed.
( -- f )
: process-glyph
  [ 0 1 stack-checker ]
  true ( f )
  8 0 do ( f )
    read-font-line if ( f )
      (pgs) .fontline @ s>c ( f c )
      (pgs) .glyph-base @ i + c! ( f )
    else
      drop false ( f )
      leave
    then
  loop
;

\ May raise x-process-font-err, x-fr-* and x-pool-* exceptions.
( tileset filename-addr filename-len --  #glyphs )
: process-font
  [ 3 1 stack-checker ]
  FA_OPEN_EXISTING FA_READ or f_open ( tileset fil )
  (pgs) .font-fp ! ( tset )
  256 [: ( tset buf )
    (pgs) .fontline ! ( tset )
    0 ( tset tidx )
    begin
      2dup swap tset-tidx>addr (pgs) .glyph-base ! ( tset tidx )
      process-glyph ( tset tidx f )
      while ( tset tidx )
        1+
    repeat
    nip ( tidx )
  ;] with-temp-allot

  (pgs) .font-fp @ f_close
;

0 0 vec2 variable cursor

( -- c )
: (edit-1)
  [ 0 1 stack-checker ]
  \ Draw the cursor
  tm mapentry{ 0 tidx cursor @ xy YELLOW fg YELLOW bg }set
  key dup case
    #27 of endof

    #8 of
      \ Erase cursor
      tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }set
      cursor @ vec2.xy ( key x y )
      swap ( key y x )
      dup if ( key y x )
        1- ( key y x-1 )
      then
      swap vec2 cursor ! ( key )
    endof

    #13 of 
      \ Erase cursor
      tm mapentry{ 0 tidx cursor @ xy YELLOW fg BLACK bg }set
      cursor @ vec2.y 1+ ( key y )
      dup #ROWS = if ( key y )
        drop 0 ( key y )
      then
      0 swap vec2 cursor ! ( key )
    endof

    $20 - ( key tidx )
    \ draw the character at the cursor position
    tm mapentry{ ( tidx ) tidx cursor @ xy YELLOW fg BLACK bg }set
    cursor @ vec2.xy swap 1+ swap ( key x y )
    over #COLS = if ( key x y )
      nip 0 swap ( key x y )
      1+ ( key x y )
      dup #ROWS = if ( key x y )
        drop 0 ( key x y )
      then
    then
    vec2 cursor ! ( key )
    dup
  endcase
;

( -- )
: vera-txt-demo
  ts tset{ 8 width 8 height 1 bpp 256 tiles }set
  tm tmap{ 64 width 32 height TMAP-TXT16 type }set
  l0 layer{ ts tset tm tmap }tilemap-mode

  cr
  ts token process-font
  ." #glyphs: " . cr

  true l0 layer-enable
  false l1 layer-enable
  false sprites-enable

  \ scale to 320x240
  $40 dup hscale! vscale!

  true display-enable

  ts tset-print
  tm tmap-print
  l0 layer-print

  begin 
    (edit-1) dup .
    27 = if exit then
    100000 0 do loop \ delay
  again
;

vera-txt-demo night-in-tokyo.fnt

