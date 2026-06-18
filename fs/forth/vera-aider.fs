\ BoxLambda Forth
\ VERA Graphics Driver

\ --- System Limits and Enumerations ---
479 constant VERA_SCANLINE_VISIBLE_MAX
524 constant VERA_SCANLINE_MAX
1023 constant VERA_HSTOP_MAX
1023 constant VERA_VSTOP_MAX
1024 constant VERA_MAX_NUM_TILES_IN_TILESET
2 constant VERA_NUM_LAYERS
2 constant VERA_NUM_SPRITE_BANKS
64 constant VERA_NUM_SPRITES_IN_BANK
VERA_NUM_SPRITE_BANKS VERA_NUM_SPRITES_IN_BANK * constant VERA_NUM_SPRITES
127 constant VERA_MAX_SPRITE_ID

32 constant VERA_NUM_MAPS
32 constant VERA_NUM_TILESETS

\ --- Color Palette Indexes ---
0 constant VERA_COLOR_BLACK
1 constant VERA_COLOR_WHITE
2 constant VERA_COLOR_RED
3 constant VERA_COLOR_CYAN
4 constant VERA_COLOR_PURPLE
5 constant VERA_COLOR_GREEN
6 constant VERA_COLOR_BLUE
7 constant VERA_COLOR_YELLOW
8 constant VERA_COLOR_ORANGE
9 constant VERA_COLOR_BROWN
10 constant VERA_COLOR_LIGHT_RED
11 constant VERA_COLOR_DARK_GREY
12 constant VERA_COLOR_GREY
13 constant VERA_COLOR_LIGHT_GREEN
14 constant VERA_COLOR_LIGHT_BLUE
15 constant VERA_COLOR_LIGHT_GREY
16 constant VERA_COLOR_GRAYSCALE_0 
31 constant VERA_COLOR_GRAYSCALE_15 

\ Map types
0 constant VERA_MAP_TYPE_TXT16
1 constant VERA_MAP_TYPE_TXT256
2 constant VERA_MAP_TYPE_TILE

\ Mask given value to 0-15 range and
\ return corresponding grayscale value in the default VERA color palette.
\ ( n -- n' )
: vera_color_grayscale
  15 and VERA_COLOR_GRAYSCALE_0 + [1-foldable]
;

\ --- Allocation Configurations ---
2048 constant VRAM_BLOCK_SZ_BYTES
9 constant LOG_VRAM_BLOCK_SZ
VERA_VRAM_SIZE_BYTES LOG_VRAM_BLOCK_SZ rshift constant VRAM_NUM_BLOCKS  

\ --- Low-Level Memory Allocator State Tracking ---
create vram_blocks_ VRAM_NUM_BLOCKS chars allot
vram_blocks_ VRAM_NUM_BLOCKS 0 fill

\ --- VRAM access words ---
: vram-wr-word ( data addr -- ) VERA_VRAM_BASE + ! [inline] ;
: vram-wr-byte ( data addr -- ) VERA_VRAM_BASE + c! [inline] ;
: vram-rd-word ( addr -- data ) VERA_VRAM_BASE + @ [inline] ;
: vram-rd-byte ( addr -- data ) VERA_VRAM_BASE + c@ [inline] ;

\\
\\ VRAM allocator functions:
\\

: x-vram-alloc-failed ." VRAM allocation failed." cr ;

\ --- VRAM Allocation Subsystem ---
\ In the vram_blocks_ array, at offset, attempt to find requested blocks.
\ Return actual number of blocks found (might be less than requested).
: (vr-find-free-blocks) ( offset requested -- found )
  >r 0 ( offset found R: requested )
  begin
    \ Stop scanning if requested number is found.
    dup r@ < while ( offset found R: requested )
      \ Stop scanning if we reached the end of vram_blocks_.
      over VRAM_NUM_BLOCKS < while ( offset found R: requested )
        \ Stop scanning if the block is not free.
        over vram_blocks_ + c@ 0= while ( offset found R: requested ) 
          \ Increment offset and found
          1 dup d+ ( offset+1 found+1 R: requested )
  repeat
  rdrop swap drop ( found )
;

\ Find a chunk of num-blocks consecutive free blocks in the vram_blocks_
\ array. Return the start index of this chunk.
\ Raise x-vram-alloc-failed exception if no chunk is found.
\ ( num-blocks -- block-idx )
: (vr-find-free-chunk)
 \ Start scanning from offset 0
  >r 0 ( block-idx R: num-blocks )

  begin
    \ stop scanning if we've reached the end of the vram_blocks_ array.
    dup VRAM_NUM_BLOCKS < while ( block-idx R: num-blocks )
      \ Attempt to find num-blocks starting from offset block-idx.
      dup r@ (vr-find-free-blocks) ( block-idx found-blocks R: num-blocks )
      \ If not found, increment offset and loop.
      r@ < while ( block-idx R: num-blocks )
        1+ ( block-idx+1 R: num-blocks )
  repeat

  rdrop ( block-idx )
  dup = VRAM_NUM_BLOCKS triggers x-vram-alloc-failed ( block-idx )
;

\ ( num-blocks block-idx -- )
: (vr-allocate-blocks)
  vram_blocks_ + ( num-blocks vram_block_ptr )
  2dup c! ( num-blocks vram_block_ptr )
  1+ swap 1- $ff fill ( )
;

: (vr-find-alloc-blocks) ( num-blocks -- block-idx )
  dup (vr-find-free-chunk) ( num-blocks block-idx )
  tuck (vr-allocate-blocks) ( block-idx )
;

\\ Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
\\ The 'init' Words use this function to allocate their resources.
\\ size-bytes: the number of bytes to allocate (use one the vera_compute* Words).
\\ If successful a 2KB-aligned Pointer to allocated block of memory in VRAM. 
\\ In not successful an x-vram-alloc-failed exception is raised.
: vram-alloc ( size-bytes -- addr )
  \\ Convert size in bytes to block size, rounding up.
  [ VRAM_BLOCK_SZ_BYTES 1- ] literal + LOG_VRAM_BLOCK_SZ rshift ( num-blocks )
  (vr-find-alloc-blocks) ( block-idx )
  \\ Convert to address
  LOG_VRAM_BLOCK_SZ lshift VERA_VRAM_BASE +
;

\ ( block-idx -- )
: (vr-free)
  vram_blocks_ + ( vram_block_ptr )
  dup c@ ( vram_block_ptr num-blocks )
  0 fill ( )
;

\\ Release VRAM allocated with vram-alloc.
: vram-free ( addr -- )
  \ Convert addr to block-idx
  VERA_VRAM_BASE - LOG_VRAM_BLOCK_SZ rshift ( block-idx )
  (vr-free)
;

\\
\\ Getting and Setting pixels in tiles:
\\

( base x y width -- ptr )
: (8bpp-pixel-byte-ptr) * + + [inline] ;

( pxlval base y width x -- )
: (pixel-set-8bpp) 
  -rot (8bpp-pixel-byte-ptr) ( pxval ptr )
  c! ( )
;

( base y width x -- pxlval )
: (pixel-set-8bpp) 
  -rot (8bpp-pixel-byte-ptr) ( ptr )
  c@ ( pxlval )
;

( base x y width -- ptr )
: (4bpp-pixel-byte-ptr) * + shr + [inline] ;

( x -- bitoffset )
: (4bpp-bitoffset-of-x) and 2 lshift [inline] ;

( pxlval base y width x -- )
: (pixel-set-4bpp) 
  dup (4bpp-bitoffset-of-x) >r ( pxlval base y width x R: bitoffset )
  -rot (4bpp-pixel-byte-ptr) ( pxval ptr R: bitoffset )
  dup c@ ( pxlval ptr oldbyte R: bitoffset )
  $f r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
  rot r> lshift or ( ptr newbyte )
  swap c! ( )
;

( base y width x -- pxlval )
: (pixel-get-2bpp)
  dup (4bpp-bitoffset-of-x) >r ( base y width x R: bitoffset )
  -rot (4bpp-pixel-byte-ptr) ( ptr R: bitoffset )
  c@ ( oldbyte R: bitoffset )
  r> rshift $f and ( pxlval )
;

( base x y width -- ptr )
: (2bpp-pixel-byte-ptr) * + 2 rshift + [inline] ;

( x -- bitoffset )
: (2bpp-bitoffset-of-x)
  3 dup rot ( 3 3 x )
  and - ( 3-x&3 )
  shl ( (3-x&3)*2 )
;

( pxlval base y width x -- )
: (pixel-set-2bpp)
  dup (2bpp-bitoffset-of-x) >r ( pxlval base y width x R: bitoffset )
  -rot (2bpp-pixel-byte-ptr) ( pxval ptr R: bitoffset )
  dup c@ ( pxlval ptr oldbyte R: bitoffset )
  3 r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
  rot r> lshift or ( ptr newbyte )
  swap c! ( )
;

( base y width x -- pxlval )
: (pixel-get-2bpp)
  dup (2bpp-bitoffset-of-x) >r ( base y width x R: bitoffset )
  -rot (2bpp-pixel-byte-ptr) ( ptr R: bitoffset )
  c@ ( oldbyte R: bitoffset )
  r> rshift 3 and ( pxlval )
;

( base x y width -- ptr )
: (1bpp-pixel-byte-ptr) * + 8/ + [inline] ;

( base width point -- ptr )
: (1bpp-pixel-byte-ptr) xy> rot * + 8/ + [inline] ;

( x -- bitoffset )
: (1bpp-bitoffset-of-x)
  7 dup rot ( 7 7 x )
  and ( 7-x&7 )
;

( pxlval point base width -- )
: (pixel-set-1bpp)
  rot dup x> (bpp-bitoffset-of-x) >r ( pxlval base width point R: bitoffset )
  (1bpp-pixel-byte-ptr) ( pxlval ptr R: bitoffset )
  dup c@ ( pxlval ptr oldbyte R: bitoffset )
  rot r> setbit ( ptr newbyte )
  swap c! ( )
;

( base y width x -- pxlval )
: (pixel-get-1bpp)
  dup (bpp-bitoffset-of-x) >r ( base y width x R: bitoffset )
  -rot (1bpp-pixel-byte-ptr) ( ptr R: bitoffset )
  c@ r> rshift 1 and ( pxlval )
;

( pxlval base y width x bpp -- )
: (pixel-set)
  \ Would a jump table be better here?
  case ( pxlval base y width x )
    1 of (pixel-set-1bpp) endof
    2 of (pixel-set-2bpp) endof
    4 of (pixel-set-4bpp) endof
    8 of (pixel-set-8bpp) endof
    true assert
  endcase
;

( x y val addr width bpp )
: (pixel-set)
  5 pick swap ( x y val addr width x bpp )
;

( base y width x bpp -- pxlval )
: (pixel-get)
  \ Would a jump table be better here?
  case ( base y width x )
    1 of (pixel-get-1bpp) endof
    2 of (pixel-get-2bpp) endof
    4 of (pixel-get-4bpp) endof
    8 of (pixel-get-8bpp) endof
    true assert
  endcase
;

\ --- Core Display Configuration API ---
: vera-display-enable ( flag -- )
  if 1 else 0 then 
  VERA_DC_VIDEO_ADDR @ [ VERA_DC_VIDEO_OUTPUT_MODE_MASK not ] literal and or VERA_DC_VIDEO_ADDR ! ;

: vera-display-enabled? ( -- flag )
  VERA_DC_VIDEO @ VERA_DC_VIDEO_OUTPUT_MODE_MASK and 1 = ;

: vera-sprites-enable ( flag -- )
  if VERA_DC_VIDEO_SPR_ENABLE_MASK else 0 then
  VERA_DC_VIDEO_ADDR @ [ VERA_DC_VIDEO_SPR_ENABLE_MASK not ] literal and or VERA_DC_VIDEO_ADDR ! ;

: vera-sprites-enabled? ( -- flag )
  VERA_DC_VIDEO_ADDR @ VERA_DC_VIDEO_SPR_ENABLE_MASK and 0<> ;

: vera-hscale-set ( scale-ufix1-7 -- ) VERA_DC_HSCALE_ADDR ! ;
: vera-hscale-get ( -- scale-ufix1-7 ) VERA_DC_HSCALE_ADDR @ ;
: vera-vscale-set ( scale-ufix1-7 -- ) VERA_DC_VSCALE_ADDR ! ;
: vera-vscale-get ( -- scale-ufix1-7 ) VERA_DC_VSCALE_ADDR @ ;
: vera-bordercolor-set ( pal-idx -- ) VERA_DC_BORDER_ADDR ! ;
: vera-bordercolor-get ( -- pal-idx ) VERA_DC_BORDER_ADDR @ ;

: vera-screen-boundaries-set ( hstart hstop vstart vstop -- )
  VERA_DC_VSTOP_ADDR ! VERA_DC_VSTART_ADDR ! VERA_DC_HSTOP_ADDR ! VERA_DC_HSTART_ADDR ! ;

: vera-screen-boundaries-get ( -- hstart hstop vstart vstop )
  VERA_DC_HSTART_ADDR @ VERA_DC_HSTOP_ADDR @ VERA_DC_VSTART_ADDR @ VERA_DC_VSTOP_ADDR @ ;

\ --- Interrupt Subsystem ---
VERA_IEN_VSYNC_MASK constant VERA_IRQ_VSYNC
VERA_IEN_LINE_MASK constant VERA_IRQ_LINE
VERA_IEN_SPRCOL_MASK constant VERA_IRQ_SPRCOL

\\ Enable IRQs. The passed in mask will be OR'd with the installed mask.
\\ @param mask: bitwise OR of VERA_IRQs to enable.
: vera-irqs-enable ( mask -- ) VERA_IEN_ADDR @ or VERA_IEN_ADDR ! ;

\\ Disable IRQs. The passed in mask will be inverted and  AND'd with the
\\ installed mask.
\\ @param mask: bitwise OR of VERA_IRQs to disable.
: vera-irqs-disable ( mask -- ) invert VERA_IEN_ADDR @ and VERA_IEN_ADDR ! ;

\\ Retrieve the enabled IRQs bitmask.
\\ @return: a bitmask of enabled VERA_IRQs.
: vera-irqs-enabled ( -- mask ) VERA_IEN_ADDR @ ;

\\ Retrieve the active IRQs.
\\ @return: a bitmask of active VERA_IRQs.
: vera-irqs-get ( -- active-mask ) VERA_ISR_ADDR @ VERA_IEN_ADDR @ and ;

\\ Acknowledge IRQs.
\\ @param mask: bitwise OR of VERA_IRQs to acknowledge.
: vera-irqs-ack ( mask -- ) $7 and VERA_ISR_ADDR ! ;

\\ Set/Get the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
\\ enabled.
\\ @param scanline: scanline number on which the trigger the line IRQ, must be
\\ <= VERA_SCANLINE_MAX.
: vera-irqline-set ( scanline -- ) VERA_IRQLINE_ADDR ! ;
: vera-irqline-get ( -- scanline ) VERA_IRQLINE_ADDR @ ;
: vera-scanline-get ( -- scanline ) VERA_SCANLINE_ADDR @ ;

\\
\\ VGA line capture Words
\\
: vera-line-capture-enable ( flag -- )
  if VERA_CTRL_STATUS_CAPTURE_EN_MASK else 0 then 
  VERA_CTRL_STATUS_ADDR @ [ VERA_CTRL_STATUS_CAPTURE_EN_MASK not ] and or VERA_CTRL_STATUS_ADDR ! ;

: vera-line-capture-enabled? ( -- flag )
  VERA_CTRL_STATUS_ADDR @ VERA_CTRL_STATUS_CAPTURE_EN_MASK and 0<> ;

\\  Read the RGB value of a pixel on the captured line.
\\ @param x: the pixel's x position. Range: 0..639.
\\ @return: 12-bit RGB triple.
: vera-line-capture-read-pixel ( x -- rgb )
  2 lshift VERA_CAPTURE_RAM_BASE + @
  $FFF and ;

\ --- Palette API ---
\ Expects standard 4-bit color fields mapped linearly
\\ Write an entry into the palette.
\\ @param idx: the palete color index
\\ @param rgb: the 12-bit RGB triple
: vera-palette-write ( idx rgb -- )
  $fff and ( idx rgb )
  swap ( rgb idx )
  2 lshift VERA_PALETTE_RAM_BASE + !
;

\\ Read the RGB value of a palette entry
\\ @param idx: the palete color index:
\\ @return: the 12-bit RGB triple
: vera-palette-read ( idx -- rgb )
  2 shift VERA_PALETTE_RAM_BASE + @
;

\ --- Tile Map API

begin-structure vera-map
  field: .map-base
  cfield: .map-log-width
  cfield: .map-log-height
  cfield: .map-type
end-structure

create vera-maps vera-map VERA_NUM_MAPS * allot
vera-maps vera-map VERA_NUM_MAPS * 0 fill

\ Calculate the map-obj-ptr of the given map
( map-idx -- map-obj-ptr )
: (map-obj-ptr) vera-map * vera-maps + [inline] ;

\ Initialize a tile map object of the given dimensions.
\ VRAM resources will be allocated
\ @param log-width: log2(width) in tiles: 5/6/7/8 (for widths: 32/64/128/256).
\ @param log-height: log2(height) in tiles: 5/6/7/8 (for heights: 32/64/128/256).
\ @param map-type: VERA_MAP_TYPE_TXT16/TXT256/TILE.
\ @param map-idx: 0..VERA_NUM_MAPS-1
\ Raises x-vram-alloc-failed exception if allocation failed.
: map-init ( log-w log-h type idx -- )
  (map-obj-ptr) >r ( log-w log-h type R: map-obj-ptr )
  \ Ensure map object is free
  r@ .map-base @ 0= assert

  \ Set map type field
  r@ .map-type c! ( log-w log-h R: map-obj-ptr )

  \ Allocate VRAM (2 * width * height ) and set map base address field.
  2dup 2 swap lshift swap lshift ( log-w log-h map-sz R: map-obj-ptr )
  vram-alloc r@ .map-base ! ( log-w log-h R: map-obj-ptr )

  \ Set log-width and log-height fields
  r@ .map-log-height c! ( log-w R: map-obj-ptr )
  r@ .map-log-width c! ( R: map-obj-ptr )

  rdrop
;

\ Deintialize a map object, releasing its VRAM resources.
: map-deinit ( map-idx -- )
  \ Look up the map base address
  (map-obj-ptr) map-base dup @ ( base-addr-field-addr base-addr )
  \ Release the VRAM (if addr field non-zero)
  ?dup if vram-free then ( base-addr-field-addr )
  \ 0 out the base-addr field
  0 swap !
;

\ returns the base address of the given map
( map-idx -- base-addr )
: map-base-addr> (map-obj-ptr) .map-base @ [inline] ;

\ return the log2(width) of the given map
( map-idx -- log-w )
: map-log-width> (map-obj-ptr) .map-log-width c@ ;

\ return the log2(height) of the given map
( map-idx -- log-h )
: map-log-height> (map-obj-ptr) .map-log-height c@ ;

\ return the type of the given map (see VER_MAP_TYPE_ constants)
( map-idx -- map-type )
: map-type> (map-obj-ptr) .map-type c@ ;

\ return true if given map is initialized
( map-idx -- f )
: map-is-initialized map-base-addr 0<> ;

\ Pack chr, fg and bg color into a 1bpp 16 color textmode map entry value
( chr fg bg - map-val)
: txtmap-entry-16
  12 lshift ( bgshifted )
  -rot 8 lshift ( bgshifted chr fgshifted )
  or or [inline]
;

\ Pack chr and fg color into a 1bpp 256 color textmode map entry value
( chr fg -- map-val )
: txtmap-entry-256
  8 lshift or [inline]
;

\ Pack tile, hflip, vflip and pal_offset into a 2/4/8bbp tile map entry value
\ The color index of tile pixels is modified by the palette offset using the
\ following logic:
\ - Color index 0 (transparent) and 16-255 are unmodified.
\ - Color index 1-15 is modified by adding 16 x palette offset.
( tile hflip vflip pal_offset -- map-val )
: tilemap-entry
  13 lshift -rot
  12 lshift -rot
  11 lshift -rot
  or or or or [inline]
;

\ Retrieve the address of the entry at row/col in given map
: map-entry-addr ( col row map-idx -- addr )
  \ Calculate map-obj-ptr
  (map-obj-ptr) r> ( col row R: map-obj-ptr )
  \ Calculate 2*(row*width_ + col)
  r@ .map-log-width @ ( col row log-w R: map-obj-ptr )
  lshift + shl ( offset R: map-obj-ptr )
  \ Add to map base address
  r@ .map-base @ ( offset base-addr R: map-obj-ptr )
  + ( entry-addr R: map-obj-ptr )
  rdrop
;

\ Write 16-bit map-entry at row/col of given map
: map-write ( data col row map-idx -- )
    map-entry-addr h! ;

\ Read 16-bit map-entry at row/col of given map
: map-read ( col row map-id -- data )
    map-entry-addr h@ ;

\ -- Tileset API
\ A tileset is used to represent tiles, sprite pixel data and bitmaps.

begin-module tilesets

  begin-structure tileset
    field:  .base
    field:  .pxl-set
    field:  .pxl-get
    hfield: .width
    hfield: .height
    hfield: .bpp
    hfield: .num-tiles
  end-structure

\ Retrieve tileset base address in VRAM.
( tileset -- addr )
: base> .base @ ;

create (valid-widths) 8 , 16 , 32 , 64 , 320 , 640 ,

( width -- f )
: (valid-width?) (valid-widths) 6 find-in 0<> ; 

\ Set the tileset width in the tileset object.
\   - 8, 16 for regular tiles.
\   - 8, 16, 32, 64 for sprites.
\   - 320, 640 for bitmaps.
( tileset width -- )
: >width
  dup (valid-width?) ?assert
  swap .width h! 
;

\ Retrieve the tileset width from the tileset object
( tileset -- width )
: width> .width h@ ;

\ Set the tileset height in the tileset object
\   - 8 or 16 for regular tiles.
\   - 8, 16, 32, 64 for sprites.
\   - Any positive value for bitmaps.
( tileset height -- )
: >height swap .height h! ;

\ Retrieve the tileset height
( tileset -- height )
: height> .height h@ ;

create (valid-bpps) 1, 2, 4, 8

( bpp -- f )
: (valid-bpp?) (valid-bpps) 4 find-in 0<> ; 

\ Set the tileset BPP in the tileset object
\   - 1, 2, 4, 8 for regular tiles and bitmaps.
\   - 4, 8 for sprites.
( tileset bpp -- )
: >bpp
  dup (valid-bpp?) ?assert
  swap >r ( bpp R: tileset )
  dup r@ .bpp h! ( tileset bpp )
  case
    1 of ['] (pixel-set-1bpp) ['] (pixel-get-1bpp) endof
    2 of ['] (pixel-set-2bpp) ['] (pixel-get-2bpp) endof
    4 of ['] (pixel-set-4bpp) ['] (pixel-get-4bpp) endof
    8 of ['] (pixel-set-8bpp) ['] (pixel-get-8bpp) endof
  endcase ( setter getter R: tileset )
  r@ .pxl-get !
  r> .pxl-set !
;

\ Retrieve the tileset BPP from the tileset object.
( tileset -- bpp )
: bpp> .bpp h@ ;

\ Set the number of tiles in the tileset.
\ Retrieve the number of tile allocated to the tileset.
\ Range: 0..1023
( tileset num -- )
: >num-tiles
  dup < 1024 ?assert
  swap .num-tiles h! 
;

( tileset -- num-tiles )
: num-tiles> .num-tiles h@ ;

\ Retrieve the tilesize in bytes for the given tileset.
( tileset -- tilesize-bytes )
: tilesize>
  dup bpp> swap dup width> swap height> * * * 8/ ;

\ (Re)Allocate VRAM for this tileset to accommodate
\ num-tiles, bpp, width and height.
\ If VRAM was previously allocated for this tileset,
\ this VRAM will be released before reallocating VRAM.
\ Throws x-vram-alloc-failed exception if VRAM allocation failed.
( tileset -- )
: vram-alloc
  dup base> ?dup if
    vram-free
  then ( tileset )
  dup .base 0 swap ! ( tileset )
  dup tilesize> swap dup num-tiles> * vram-alloc ( tileset addr )
  swap .base ! ( )
;

\ Get a pointer to the pixel data of a tile in the tileset.
\ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
\ @param tileset: Tileset object
 ( tileset tile-idx -- addr )
: tile-addr over tilesize> * swap base> + ;

\ Get a tile descriptor from the tileset.
\ A tile descriptor is a double consisting of the tuple (tileset tile-addr).
\ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
\ @param tileset: Tileset object
 ( tileset tile-idx -- tileset addr )
: tile 
  over tilesize> * ( tileset tileoffset )
  over base> + ( tileset addr )
;

  begin-module point
    ( x y -- point )
    : >xy $ffff and swap 16 lshift or ;

    ( point -- x y )
    : xy> dup 16 rshift swap and $ffff ;

    ( x point -- )
    : >x $ffff and swap 16 rshift or [inline] ;

    ( point -- x )
    : x> 16 rshift [inline] ;
    
    ( dx point -- )
    : x+ swap 16 lshift + [inline] ;

    ( y point -- )
    : >y $ffff0000 and swap $ffff and or [inline] ;

    ( point -- y )
    : y> $ffff and [inline] ;

    ( dy point -- )
    : y+ 
      tuck + $ffff and ( point y )
      swap $ffff0000 and or 
    ;
  end-module

  begin-module tile
      \ Set a pixel in the given tile.
      \ A tile descriptor is a double consisting of the tuple (tileset tile-addr).
      ( pxlval point tile -- )
    : pixel-set
      over width> ( pxlval point tileset base width )
      rot .pxl-set @ ( pxlval point base width pxl-set )
      execute
    ;

  end-module

  begin-module pixel

    begin-structure pxl
      hfield: .x
      hfield: .y
      field: .val
    end-structure

    \ Set pixel x coordinate
    ( pxl x -- )
    : >x swap .x h! [inline] ;

    \ Retrieve pixel x coordinate
    ( pxl -- x )
    : x> .x h@ [inline] ;
 
    \ Set pixel y coordinate
    ( pxl y -- )
    : >y swap .y h! [inline] ;

    \ Retrieve pixel y coordinate
    ( pxl -- y )
    : y> .y h@ [inline] ;

    \ Set pixel value
    ( pxl val -- )
    : >val swap .val ! [inline] ;

    \ Retrieve pixel value
    ( pxl -- val )
    : val> .val @ [inline] ;

    \ Set pixel x, y and value
    ( pxl x y val -- )
    : set
      3 pick >r ( pixel x y val R: pixel )
      r@ .val ! ( pixel x y R: pixel )
      r@ .y h! ( pixel x R: pixel )
      r> .x h! ( pixel )
      drop ;
 
    \ Get pixel x, y and value
    ( pxl -- x y val )
    : get
      >r
      r@ .x h@
      r@ .y h@
      r> .val ! ;
  end-module

  \ Set a pixel in a given tile of this tileset.
  ( pixel tileset tile-idx -- )
  : tileset-pixel-set
    rot >r ( tileset tile-idx R: pixel )
    2dup tile-addr nip swap ( addr tileset R: pixel )

    r@ pixel::val> -rot ( val addr tileset R: pixel )
    r@ pixel::y> swap ( val addr y tileset R: pixel )
    dup .width swap ( val addr y width tileset R: pixel )
    r> pixel::x> swap ( val addr y width x tileset )
    .bpp ( val addr y width x bpp )
    (pixel-set)
  ;


end-module

\ Get the pixel value in a given tile.
\ @param x: pixel x-position. Range: 0..width-1.
\ @param y: pixel y-position. Range: 0..height-1.
\ @param tile-idx: tile index. Range 0..num_tiles-1.
\ @param tileset-idx: Tileset identifier
\ @return pxlval: pixel value. Range: 0..(2^bpp)-1.
( x y tile-idx tileset-idx -- pxlval )
: tileset-pixel-get
  (tileset-obj-ptr) >r ( x y tile-idx R: objptr )
  r@ (tileset-tile-data-addr) ( x y tilebase R: objptr )
  -rot swap ( tilebase y x R: objptr )
  r@ .tileset-width h@ swap ( tilebase y width x R: objptr )
  r> .tileset-bpp c@ ( tilebase y width x bpp )
  (pixel-get) ( pxlval )
;

\ --- Sprite API ---
begin-module sprites

  begin-structure sprite 
    field:  .attr-ram
    hfield: .attr-addr
    hfield: .attr-x
    hfield: .attr-y
    hfield: .attr-flags
  end-structure

  : init ( sprite -- ) sprite 0 fill ;

  \ Calculate the sprite attribute RAM address from the given sprite id.
  ( id -- addr )
  : (id>ram) 8 * VERA_SPRITE_RAM_BASE + [inline] ;

  \ Calculate the sprite id from the given sprite attribute RAM address.
  ( addr -- id )
  : (ram>id) VERA_SPRITE_RAM_BASE -  [inline] ;

  \ Set the sprite id in the sprite object.
  : >id ( sprite id -- ) (id>ram) swap .attr-ram ! ;

  \ Retrieve the sprite id from the sprite object.
  : id> ( sprite -- id ) .attr-ram @ (ram>id) ;

  \ Set the sprite x position in the sprite object.
  : >x ( sprite x -- ) swap .attr-x h! ;

  \ Retrieve the sprite x position from the sprite object.
  : x> ( sprite -- x ) .attr-x h@ ;

  \ Set the sprite y position in the sprite object.
  : >y ( sprite y -- ) swap .attr-y h! ;

  \ Retrieve the sprite y position from the sprite object.
  : y> ( sprite -- y ) .attr-y h@ ;

  \ Set the tile to be used in the sprite object.
  : >tile ( sprite tileset tile-idx -- )
  ;
end-module

: sprite
  dup (sprite-obj-ptr) >r ( idx R: obj )
  r@ vera-sprite 0 fill ( idx R: obj )
  dup r@ .sprite-idx ! ( idx R: obj )
  0 (sprite-attrs) ( attrs-addr R: obj )
  0 swap 2dup ! cell+ 2dup ! cell+ 2dup ! cell+ ! ( R: obj )
  VERA_TILESET_ID_UNKNOWN r@ .sprite-tileset-idx c! ( R: obj )
  VERA_SPR_Z_DIS r> .sprite-z-depth c!
;
begin-structure vera-sprite
  hfield: .sprite-x
  hfield: .sprite-y
  hfield: .sprite-tile-idx
  cfield: .sprite-tileset-idx
  cfield: .sprite-idx
  cfield: .sprite-pal-offset
  cfield: .sprite-collision-mask
  cfield: .sprite-z-depth
  cfield: .sprite-v-h-flip
end-structure

create vera-sprites vera-sprite VERA_NUM_SPRITES * allot

\ Calculate sprite object pointer from the given index
( sprite-idx -- sprite-obj-ptr )
: (sprite-obj-ptr) * vera-sprits vera-sprite + [inline] ;

\ Calculate the sprite attributes address from the given sprite index and attribute index
( attr-idx sprite-idx -- attrs-addr )
: (sprite-attrs) 8 * VERA_SPRITE_RAM_BASE + ( attr-idx attrs-base ) 
  swap 2* + [inline] ;

: sprite-init ( sprite-idx -- )
  dup (sprite-obj-ptr) >r ( idx R: obj )
  r@ vera-sprite 0 fill ( idx R: obj )
  dup r@ .sprite-idx ! ( idx R: obj )
  0 (sprite-attrs) ( attrs-addr R: obj )
  0 swap 2dup ! cell+ 2dup ! cell+ 2dup ! cell+ ! ( R: obj )
  VERA_TILESET_ID_UNKNOWN r@ .sprite-tileset-idx c! ( R: obj )
  VERA_SPR_Z_DIS r> .sprite-z-depth c!
;

1 constant VERA_SPR_X_ATTR
2 constant VERA_SPR_X_ATTR

: sprite-set-x ( x sprite-idx -- )
  >r ( x R: idx )
  dup r@ (sprite-obj-ptr) .sprite-x h! ( x R: idx )
  VERA_SPR_X_ATTR r> (sprite-attrs) h! ( )
;

: sprite-set-y ( y sprite-idx -- )
  >r ( y R: idx )
  dup r@ (sprite-obj-ptr) .sprite-x h! ( y R: idx )
  VERA_SPR_Y_ATTR r> (sprite-attrs) h! ( )
;

\ ( bpp addr -- addr-attr )
: (sprite-build-addr-attr)
  VERA_VRAM_BASE - 5 rshift ( bpp addr )
  swap 8 = if ( addr )
    VERA_SPRITE_ATTR_ADDR_MODE_MASK or 
  then
;

: sprite-tile-set ( tile-idx tileset-idx sprite-idx -- )
  (sprite-obj-ptr) >r ( tile-idx tileset-idx R: obj )
  2dup
  r@ .sprite-tileset-idx c! ( tile-idx tileset-idx tile-idx R: obj )
  r@ .sprite-tile-idx h! ( tile-idx tileset-idx R: obj ) 

  tuck ( tileset-idx tile-idx tileset-idx R: obj )
  tileset-tile-data-addr ( tileset-idx tiledata-addr R: obj )
  tileset-bpp> ( bpp tiledate R: obj )
    >r over over tile-data-addr VERA_VRAM_BASE - 5 rshift ( compute base word )
    2 pick cells tileset-bpps + @ 8 = if $8000 or then ( handle 8bpp configuration bit )
    r@ 8 * VERA_SPRITE_RAM_BASE + ! ( store attribute word 0 )
    
    \ Pull internal geometries to parse sizing configuration masks
    2 pick cells tileset-widths + @ (tile-sz-enc) 12 lshift
    3 pick cells tileset-heights + @ (tile-sz-enc) 14 lshift or
    \ Setup configuration fields defaults to hidden depth, base palette offset 0
    r> 8 * VERA_SPRITE_RAM_BASE + 6 + !
    drop drop ;

\ Direct byte manipulation interfaces for Sprite registers configuration 6 and 7
: sprite-config-flags-set ( hflip vflip z-depth col-mask sprite-id -- )
    8 * VERA_SPRITE_RAM_BASE + 6 + ( address of configuration word 3 )
    >r swap 4 lshift swap 2 lshift or swap 1 lshift or or ( clear flags assembly )
    r@ c@ $00 and or r> c! ;

: sprite-palette-offset-set ( pal-offset sprite-id -- )
    8 * VERA_SPRITE_RAM_BASE + 7 + c! ;

\ --- Functional Layer Setup API ---
: layer-enable ( flag layer-id -- )
    0= if \ Layer 0 configuration tracking inside DC_VIDEO
        if $40 else 0 then VERA_DC_VIDEO @ $BF and or VERA_DC_VIDEO !
    else \ Layer 1 configuration tracking inside DC_VIDEO
        if $20 else 0 then VERA_DC_VIDEO @ $DF and or VERA_DC_VIDEO !
     marriage-flag then ;

: layer-enabled? ( layer-id -- flag )
    0= if VERA_DC_VIDEO @ $40 and else VERA_DC_VIDEO @ $20 and then 0= 0= ;

: layer-regs-base ( layer-id -- reg-addr )
    if VERA_L1_CONFIG else VERA_L0_CONFIG then ;

: layer-hscroll-set ( offset layer-id -- ) layer-regs-base $10 + ! ;
: layer-hscroll-get ( layer-id -- offset ) layer-regs-base $10 + @ ;
: layer-vscroll-set ( offset layer-id -- ) layer-regs-base $14 + ! ;
: layer-vscroll-get ( layer-id -- offset ) layer-regs-base $14 + @ ;

: layer-map-set ( map-id layer-id -- )
    >r dup map-types swap cells + @ 1 = if 1 else 0 then 16 lshift
    over map-widths swap cells + @ (tile-sz-enc) 4 lshift or
    over map-heights swap cells + @ (tile-sz-enc) 6 lshift or
    r@ layer-regs-base ! ( CONFIG reg register setup )
    
    swap cells map-bases + @ VERA_VRAM_BASE - 9 rshift
    r> layer-regs-base 4 + ! ( MAPBASE reg register setup ) ;

: layer-tileset-set ( tileset-id layer-id -- )
    >r
    dup cells tileset-bpps + @
    case
        1 of 0 endof
        2 of 1 endof
        4 of 2 endof
        8 of 3 endof
    endcase
    r@ layer-regs-base ! ( CONFIG write set color depth )
    
    dup cells tileset-widths + @ 16 = if 1 else 0 then
    over cells tileset-heights + @ 16 = if 2 else 0 then or
    swap cells tileset-bases + @ VERA_VRAM_BASE - 11 rshift 5 lshift or
    r> layer-regs-base 8 + ! ( TILEBASE set layout mappings ) ;


