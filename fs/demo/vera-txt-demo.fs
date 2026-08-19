: x-process-fontline-len-err ." Invalid line length in font file error" cr ;
: x-process-font-dot-star-err ." Font file ./* error" cr ;
: x-process-font-eof-err ." Unexpected EOF error processing font file" cr ;

0 constant PROCESS-GLYPH_STAT-OK
1 constant PROCESS-GLYPH_STAT-ERR
2 constant PROCESS-GLYPH_STAT-EOF

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
    swap drop ( len )
    dup (pgs) .fontlinelen ! ( len )
    0= triggers x-process-font-eof-err ( )
    (pgs) .fontline @ c@ 
    dup [char] # <> ( char f )
    over #13 <> ( char f f )
    rot #10 <> and and ( f )
  until
  (pgs) .fontlinelen @ 8 < ( f )
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
( tileset filename-addr filename-len -- )
: process-font
  [ 3 0 stack-checker ]
  FA_OPEN_EXISTING FA_READ or f_open ( tileset fil )
  (pgs) .font-fp ! ( tileset )
  256 [:
    (pgs) .fontline ! ( tileset )
    0 ( tileset tidx )
    begin
      process-glyph ( tset tidx f )
      while ( tset tidx )
        2dup swap tset-tidx>addr (pgs) .glyph-base ! ( tset tidx )
        1+
    repeat
    2drop ( )
  ;] with-temp-allot

  (pgs) .font-fp @ f_close
;

0 0 vec2 variable cursor

: edit-loop
  [ 0 0 stack-checker ]
  begin ( )
    \ Draw the cursor
    tm mapentry{ [char] _ tidx cursor @ xy WHITE fg BLUE bg }set
    key ( key )
    dup #13 = if
      drop
      cursor @ vec2.y 1+ ( y )
      dup #60 = if ( y )
        drop 0 ( y )
      then
      0 swap vec2 cursor !
    else ( key )
      $20 -
      \ draw the character at the cursor position
      tm mapentry{ ( key ) tidx cursor @ xy YELLOW fg BLUE bg }set
      cursor @ vec2.xy swap 1+ swap ( x y )
      over #80 = if ( x y )
        nip 0 swap ( x y )
        1+ ( x y )
        dup #60 = if ( x y )
          drop 0 ( x y )
        then
      then
      vec2 cursor !
    then
    \ delay
    100000 0 do loop
  again
;

ts tset{ 8 width 8 height 1 bpp 256 tiles }set
tm tmap{ 128 width 64 height TMAP-TXT16 type }set
l0 layer{ ts tset tm tmap }tilemap-mode

ts s" altered-chrome.fnt" process-font

true l0 layer-enable
false l1 layer-enable
false sprites-enable
true display-enable

edit-loop

