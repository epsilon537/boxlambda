\ BoxLambda Forth
\ VERA font loader module.

: x-process-font-dot-star-err ." Font file ./* error" cr ;

\ Process glyph object structure
begin-structure process-glyph-struct
  field: .glyph-base \ Address of glyph (character data) being process in VRAM.
  field: .font-fp \ .fnt font file pointer.
  field: .fontline \ Pointer to temp buffer holding one line of the font file.
  field: .fontlinelen \ Length of the line.
end-structure

\ process-glyph-struct singleton instance.
create (pgs) process-glyph-struct allot

\ Convert a line of 8 . and/or * to a byte
\ E.g. ...**... -> $18
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

\ Read one font line from the font file into the .fontline buffer.
\ True means a complete fontline has been read.
( -- f )
: read-font-line
  [ 0 1 stack-checker ]
  begin
    (pgs) .font-fp @ (pgs) .fontline @ 256 f_gets ( addr len )
    nip ( len )
    dup (pgs) .fontlinelen ! ( len )
    0= if true
    else
      \ Skip empty lines and lines starting with #.
      (pgs) .fontline @ c@ ( char )
      dup [char] # <> ( char f )
      over #13 <> ( char f f )
      rot #10 <> ( f f f )
      and and ( f )
    then
  until
  (pgs) .fontlinelen @ 8 >= ( f )
;

\ Process one 8x8 glyph, i.e. read the glyph definition
\ from the font file, convert it to 8 bytes and store at
\ the glyph's position in VRAM.
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

\ Load font file, process all glyphs and store in given VERA tileset.
\ May raise x-process-font-err, x-fr-* and x-pool-* exceptions.
( tileset filename-addr filename-len --  #glyphs )
: load-font
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

