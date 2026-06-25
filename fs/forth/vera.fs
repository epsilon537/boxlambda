\ BoxLambda Forth
\ VERA Graphics Driver

begin-module vera

  \ --- System Limits and Enumerations ---
  #479 constant SCANLINE_VISIBLE_MAX
  #524 constant SCANLINE_MAX
  #1023 constant HSTOP_MAX
  #1023 constant VSTOP_MAX
  #1024 constant MAX_NUM_TILES_IN_TILESET
  #2 constant NUM_LAYERS
  #2 constant NUM_SPRITE_BANKS
  #64 constant NUM_SPRITES_IN_BANK
  NUM_SPRITE_BANKS NUM_SPRITES_IN_BANK * constant NUM_SPRITES
  #127 constant MAX_SPRITE_ID

  begin-module point
    ( x y -- point )
    : <point> 16 lshift swap $ffff and or [inline] ;

    ( point -- x y )
    : xy> dup and $ffff swap 16 rshift [inline] ;

    ( y point -- )
    : >y $ffff and swap 16 rshift or [inline] ;

    ( point -- y )
    : y> 16 rshift [inline] ;
    
    ( dy point -- )
    : +>y swap 16 lshift + [inline] ;

    ( x point -- )
    : >x $ffff0000 and swap $ffff and or [inline] ;

    ( point -- x )
    : x> $ffff and [inline] ;

    ( dx point -- )
    : +>x
      tuck + $ffff and ( point y )
      swap $ffff0000 and or 
    ;
  end-module

  \ --- VRAM ---

  begin-module vram

    #2048 constant BLOCK_SZ_BYTES
    #9 constant LOG_BLOCK_SZ
    VERA_VRAM_SIZE_BYTES LOG_BLOCK_SZ rshift constant NUM_BLOCKS

    create blocks_ NUM_BLOCKS chars allot
    blocks_ NUM_BLOCKS 0 fill

    : wr-word ( data addr -- ) VERA_VRAM_BASE + ! [inline] ;
    : wr-byte ( data addr -- ) VERA_VRAM_BASE + c! [inline] ;
    : rd-word ( addr -- data ) VERA_VRAM_BASE + @ [inline] ;
    : rd-byte ( addr -- data ) VERA_VRAM_BASE + c@ [inline] ;

    : x-alloc-failed ." VRAM allocation failed." cr ;

    \ --- VRAM Allocation Subsystem ---
    \ In the vram_blocks_ array, at offset, attempt to find requested blocks.
    \ Return actual number of blocks found (might be less than requested).
    : (find-free-blocks) ( offset requested -- found )
      >r 0 ( offset found R: requested )
      begin
        \ Stop scanning if requested number is found.
        dup r@ < while ( offset found R: requested )
          \ Stop scanning if we reached the end of vram_blocks_.
          over NUM_BLOCKS < while ( offset found R: requested )
            \ Stop scanning if the block is not free.
            over blocks_ + c@ 0= while ( offset found R: requested ) 
              \ Increment offset and found
              1 dup d+ ( offset+1 found+1 R: requested )
      repeat
      rdrop swap drop ( found )
    ;

    \ Find a chunk of num-blocks consecutive free blocks in the vram_blocks_
    \ array. Return the start index of this chunk.
    \ Raise x-vram-alloc-failed exception if no chunk is found.
    \ ( num-blocks -- block-idx )
    : (find-free-chunk)
      \ Start scanning from offset 0
      >r 0 ( block-idx R: num-blocks )
      begin
        \ stop scanning if we've reached the end of the vram_blocks_ array.
        dup NUM_BLOCKS < while ( block-idx R: num-blocks )
          \ Attempt to find num-blocks starting from offset block-idx.
          dup r@ (find-free-blocks) ( block-idx found-blocks R: num-blocks )
          \ If not found, increment offset and loop.
          r@ < while ( block-idx R: num-blocks )
            1+ ( block-idx+1 R: num-blocks )
      repeat

      rdrop ( block-idx )
      dup = NUM_BLOCKS triggers x-alloc-failed ( block-idx )
    ;

    ( num-blocks block-idx -- )
    : (allocate-blocks)
      blocks_ + ( num-blocks vram_block_ptr )
      2dup c! ( num-blocks vram_block_ptr )
      1+ swap 1- $ff fill ( )
    ;

    : (find-alloc-blocks) ( num-blocks -- block-idx )
      dup (find-free-chunk) ( num-blocks block-idx )
      tuck (allocate-blocks) ( block-idx )
    ;

    \ Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
    \ The 'init' Words use this function to allocate their resources.
    \ size-bytes: the number of bytes to allocate (use one the vera_compute* Words).
    \ If successful a 2KB-aligned Pointer to allocated block of memory in VRAM. 
    \ In not successful an x-vram-alloc-failed exception is raised.
    : alloc ( size-bytes -- addr )
      \ Convert size in bytes to block size, rounding up.
      [ BLOCK_SZ_BYTES 1- ] literal + LOG_BLOCK_SZ rshift ( num-blocks )
      (find-alloc-blocks) ( block-idx )
      \ Convert to address
      LOG_BLOCK_SZ lshift VERA_VRAM_BASE +
    ;

    \ ( block-idx -- )
    : (free)
      blocks_ + ( vram_block_ptr )
      dup c@ ( vram_block_ptr num-blocks )
      0 fill ( )
    ;

    \ Release VRAM allocated with vram-alloc.
    : free ( addr -- )
      \ Convert addr to block-idx
      VERA_VRAM_BASE - LOG_BLOCK_SZ rshift ( block-idx )
      (free)
    ;
  end-module

  \ --- Tile Map API

  begin-module tilemaps

    begin-structure map
      field:  .base
      hfield: .width
      hfield: .height
      cfield: .type
    end-structure

    \ Initialize the map object. This must be done only once.
    \ ( map -- )
    : init map 0 fill ;

    \ Set map width in the map object: 32, 64, 128, 256
    \ ( width map -- )
    : width! .width h! ;

    \ Retrieve map width from the map object.
    \ ( map -- width )
    : width@ .width h@ ;

    \ Set map height in the map object: 32, 64, 128, 256
    \ ( height map -- )
    : height! .height h! ;

    \ Retrieve map height from the map object.
    \ ( map -- height )
    : height@ .height h@ ;

    \ Map types
    begin-module type
      0 constant TXT16
      1 constant TXT256
      2 constant TILE
    end-module

    \ Set the map type : type :: TXT16/TXT256/TILE.
    \ ( type map -- )
    : type! .type c! ;

    \ Retrieve the map type from the map object
    \ ( map -- type )
    : type@ .type c@ ;

    \ Retrieve tilemap base address in VRAM.
    \ ( tilemap -- addr )
    : base@ .base @ ;

    \ (Re)Allocate VRAM for this tilemap to accommodate the width and height
    \ If VRAM was previously allocated for this tilemap,
    \ this VRAM will be released before reallocating VRAM.
    \ Throws x-vram-alloc-failed exception if VRAM allocation failed.
    \ ( map -- )
    : vram-alloc
      dup base@ ?dup if
        vram :: free
      then ( map )
      dup .base 0 swap ! ( map )
      \ Allocate VRAM (2 * width * height ) and set map base address field.
      dup width@ ( map width )
      over height@ ( map width height )
      * 2* ( map sz )
      vram :: alloc ( map vram )
      swap .base !
    ;

    \ Get the address of the entry at point in given map
    : point>addr ( point map -- addr )
      \ Calculate 2*(row*width_ + col)
      2dup width@ swap point :: y>  * ( point map y*w )
      rot point :: x> + 2* ( map offset )
      swap base@ +
    ;

    \ Write 16-bit value at point of given map
    : at-point! ( mapentry point map -- ) point>addr h! ;

    \ Read 16-bit value at point of given map
    : at-point@ ( point map -- mapentry ) point>addr h@ ;

    \ Pack chr, fg and bg color into a 1bpp 16 color textmode map entry value
    \ ( chr fg bg - mapentry )
    : pack-txt16
      12 lshift ( bgshifted )
      -rot 8 lshift ( bgshifted chr fgshifted )
      or or
    ;

    \ Unpack chr, fg and bg color from a 1bpp 16 color textmode map entry value
    \ ( mapentry - chr fg bg )
    : unpack-txt16
      dup $ff and ( mapentry chr )
      swap dup 8 rshift $f and ( chr mapentry fg )
      swap 12 rshift $f and ( chr fg bg )
    ;

    \ Pack chr and fg color into a 1bpp 256 color textmode map entry value
    \ ( chr fg -- mapentry )
    : pack-txt256
      8 lshift or
    ;

    \ UnPack chr and fg color from a 1bpp 256 color textmode map entry value
    \ ( mapentry -- chr fg )
    : unpack-txt256
      dup $ff and ( mapentry chr )
      swap 8 rshift $ff and ( chr fg )
    ;

    \ Pack tile, hflip, vflip and pal_offset into a 2/4/8bbp tile map entry value
    \ The color index of tile pixels is modified by the palette offset using the
    \ following logic:
    \ - Color index 0 (transparent) and 16-255 are unmodified.
    \ - Color index 1-15 is modified by adding 16 x palette offset.
    \ ( tile-idx hflip vflip pal_offset -- mapentry )
    : pack-tile
      12 lshift -rot
      11 lshift -rot
      10 lshift -rot
      or or or or
    ;

    \ Unpack tile, hflip, vflip and pal_offset from a 2/4/8bbp tile map entry value
    \ The color index of tile pixels is modified by the palette offset using the
    \ following logic:
    \ - Color index 0 (transparent) and 16-255 are unmodified.
    \ - Color index 1-15 is modified by adding 16 x palette offset.
    \ ( mapentry -- tile-idx hflip vflip paloffset )
    : unpack-tile
      dup $3ff and ( mapentry tile-idx )
      swap 10 rshift 1 and ( tile-idx mapentry hflip )
      swap 11 rshift 1 and ( tile-idx hflip mapentry vflip )
      swap 12 rshift $f and ( tile-idx hflip vflip paloffset )
    ;
  end-module

  \ -- Tileset API
  \ A tileset is used to represent tiles, sprite pixel data and bitmaps.

  begin-module tilesets

    begin-structure tileset
      field:  .base
      field:  .pxl!
      field:  .pxl@
      hfield: .width
      hfield: .height
      hfield: .bpp
      hfield: .num-tiles
    end-structure

    \ Initialize the tileset object. This must be done only once.
    : init ( tileset -- ) tileset 0 fill ;

    \ Retrieve tileset base address in VRAM.
    ( tileset -- addr )
    : base@ .base @ ;

    \ Set the tileset width in the tileset object.
    \   - 8, 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - 320, 640 for bitmaps.
    ( width tileset -- )
    : width!
      .width h! 
    ;

    \ Retrieve the tileset width from the tileset object
    ( tileset -- width )
    : width@ .width h@ ;

    \ Set the tileset height in the tileset object
    \   - 8 or 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - Any positive value for bitmaps.
    ( height tileset -- )
    : height! .height h! ;

    \ Retrieve the tileset height
    ( tileset -- height )
    : height@ .height h@ ;

    \ Set the tileset BPP in the tileset object
    \   - 1, 2, 4, 8 for regular tiles and bitmaps.
    \   - 4, 8 for sprites.
    ( bpp tileset -- )
    : bpp!
      2dup .bpp h! ( bpp tileset )
      swap case
        1 of ['] pixel :: 1bpp! ['] pixel :: 1bpp@ endof
        2 of ['] pixel :: 2bpp! ['] pixel :: 2bpp@ endof
        4 of ['] pixel :: 4bpp! ['] pixel :: 4bpp@ endof
        8 of ['] pixel :: 8bpp! ['] pixel :: 8bpp@ endof
      endcase ( tileset setter getter )
      -rot over ( getter tileset setter tileset )
      .pxl! !
      .pxl@ !
    ;

    \ Retrieve the tileset BPP from the tileset object.
    ( tileset -- bpp )
    : bpp@ .bpp h@ ;

    \ Set the number of tiles in the tileset.
    \ Retrieve the number of tiles allocated to the tileset.
    \ Range: 0..1023
    ( num tileset -- )
    : num-tiles!
      over < 1024 ?assert
      .num-tiles h! 
    ;

    ( tileset -- num-tiles )
    : num-tiles@ .num-tiles h@ ;

    \ Retrieve the tilesize in bytes for the given tileset.
    ( tileset -- tilesize-bytes )
    : tilesize@ dup bpp@ swap dup width@ swap height@ * * * 8/ ;

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
    ( tile-idx tileset -- addr )
    : (bitmap-ptr) dup tilesize@ rot * ( tileset tilesize*tile-idx ) swap base@ + ;

    \ Get a bitmap descriptor from the tileset.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-ptr).
    \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
    \ @param tileset: Tileset object
    ( tile-idx tileset -- bitmap-desc )
    : >bitmap-descr tuck (bitmap-ptr) [inline] ;
  end-module

  \
  \ Getting and Setting pixels in tiles:
  \
  begin-module pixel

    ( base width point -- ptr )
    : (8bpp-byte-ptr) point :: xy@ rot * + + [inline] ;

    ( pxlval point base width -- )
    : 8bpp! 
      rot (8bpp-byte-ptr) ( pxval ptr )
      c! ( )
    ;

    ( point base width -- pxlval )
    : 8bpp@ 
      rot (8bpp-byte-ptr) ( ptr )
      c@ ( pxlval )
    ;

    ( base width point -- ptr )
    : (4bpp-byte-ptr) point :: xy@ rot * + 2/ + [inline] ;

    ( x -- bitoffset )
    : (4bpp-x-bitoffset) and 2 lshift [inline] ;

    ( pxlval point base -- )
    : 4bpp! 
      rot dup point :: x@ (4bpp-x-bitoffset) >r ( pxlval base y width point R: bitoffset )
      (4bpp-byte-ptr) ( pxval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      $f r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
      rot r> lshift or ( ptr newbyte )
      swap c! ( )
    ;

    ( point base width -- pxlval )
    : 4bpp@
      rot dup (4bpp-x-bitoffset) >r ( base width point R: bitoffset )
      (4bpp-byte-ptr) ( ptr R: bitoffset )
      c@ ( oldbyte R: bitoffset )
      r> rshift $f and ( pxlval )
    ;

    ( base width point -- ptr )
    : (2bpp-byte-ptr) point :: xy@ rot * + 4/ + [inline] ;

    ( x -- bitoffset )
    : (2bpp-x-bitoffset)
      3 dup rot ( 3 3 x )
      and - ( 3-x&3 )
      shl ( (3-x&3)*2 )
    ;

    ( pxlval point base width -- )
    : 2bpp!
      rot dup point :: x@ (2bpp-x-bitoffset) >r ( pxlval base width point R: bitoffset )
      (2bpp-byte-ptr) ( pxval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      3 r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
      rot r> lshift or ( ptr newbyte )
      swap c! ( )
    ;

    ( point base width -- pxlval )
    : 2bpp@
      rot dup (2bpp-x-bitoffset) >r ( base width point R: bitoffset )
      (2bpp-byte-ptr) ( ptr R: bitoffset )
      c@ ( oldbyte R: bitoffset )
      r> rshift 3 and ( pxlval )
    ;

    ( base width point -- ptr )
    : (1bpp-byte-ptr) point :: xy@ rot * + 8/ + [inline] ;

    ( x -- bitoffset )
    : (1bpp-x-bitoffset) 7 dup rot ( 7 7 x ) and ( 7-x&7 ) [inline] ;

    ( pxlval point base width -- )
    : 1bpp!
      rot dup x> (1bpp-x-bitoffset) >r ( pxlval base width point R: bitoffset )
      (1bpp-byte-ptr) ( pxlval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      rot r> setbit ( ptr newbyte )
      swap c! ( )
    ;

    ( point base width -- pxlval )
    : 1bpp@
      rot dup (bpp-x-bitoffset) >r ( base width point R: bitoffset )
      (1bpp-byte-ptr) ( ptr R: bitoffset )
      c@ r> rshift 1 and ( pxlval )
    ;
 
    \ Set a pixel in the given tile.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-ptr).
    \ ( pxlval point bitmap-descr -- )
    : pxl!
      over tileset :: width@ ( pxlval point tileset addr width )
      rot tileset :: .pxl! @ ( pxlval point addr width pxl-setter )
      execute
    ;

    \ Get a pixel from the given tile.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-ptr).
    \ ( point bitmap-descr -- pxlval )
    : pxl@
      over tileset :: width@ ( point tileset addr width )
      rot tileset :: .pxl-get @ ( point addr width pxl-getter )
      execute
    ;
  end-module

  \ --- Sprite API ---

  begin-module sprites

    begin-structure sprite 
      field:  .tileset
      field:  .attr-ram-ptr
      hfield: .attr-addr
      hfield: .attr-x
      hfield: .attr-y
      hfield: .attr-flags
    end-structure

    : init ( sprite -- ) sprite 0 fill ;

    \ Calculate the sprite attribute RAM address from the given sprite id.
    \ ( id -- addr )
    : (id>ram) 8 * VERA_SPRITE_RAM_BASE + [inline] ;

    \ Calculate the sprite id from the given sprite attribute RAM address.
    \ ( addr -- id )
    : (ram>id) VERA_SPRITE_RAM_BASE -  [inline] ;

    \ Set the sprite id in the sprite object.
    : id! ( id sprite -- ) swap (id>ram) swap .attr-ram-ptr ! ;

    \ Retrieve the sprite id from the sprite object.
    : id@ ( sprite -- id ) .attr-ram @ (ram>id) ;

    \ Get the sprite's current coordinates.
    \ ( sprite -- point )
    : point@ dup .attr-x h@ swap .attr-y h@ point :: <point> ;

    \ ( point sprite -- )
    : point!
        swap ( sprite point )
        2dup point :: x@ swap .attr-x h!
        point :: y@ swap .attr-y h!
    ;

    \ ( tilesize - tilesize-encoded )
    : sizeenc log2 3 - [inline] ;

    \ ( tilesize-encoded -- tilesize )
    : sizedec 3 + 1<< [inline] ;

    \ Set the sprite width
    \ ( width sprite -- )
    : width! swap (sizeenc) swap .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH! ;

    \ Get the sprite width
    \ ( sprite -- width )
    : width@ .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH@ (sizedec) ;

    \ Set the sprite height
    \ ( height sprite -- )
    : height!
      swap (sizeenc) swap .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT! ;

    \ Get the sprite height
    \ ( sprite -- height )
    : height@
      .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT@ (sizedec) ;

    \ ( vflip sprite -- )
    : vflip! .attr-flags VERA_SPRITE_ATTR_FLAGS_VFLIP! ;

    \ ( sprite -- f )
    : vflip@ .attr-flags VERA_SPRITE_ATTR_FLAGS@ 0<> ;

    \ ( hflip sprite -- )
    : hflip! .attr-flags VERA_SPRITE_ATTR_FLAGS_HFLIP! ;

    \ ( sprite -- f )
    : hflip@ .attr-flags VERA_SPRITE_ATTR_FLAGS_H_FLIP@ 0<> ;

    begin-module zdepth
      VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS constant DIS \ Sprite disabled. 
      VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 constant BG_L0 \ Between background and L0. 
      VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 constant L0_L1 \ Between L0 and L1. 
      VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 constant L1 \ In front of L1. 
    end-module

    \ ( zdepth sprite -- )
    : zdepth! .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH! ; ;

    \ ( sprite -- f )
    : zdepth@ .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH@ 0<> ;

    \ ( colmask sprite -- )
    : colmask! .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK! ;

    \ ( sprite -- colmask )
    : colmask@ .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK@ ;

    \ ( paloffset sprite -- )
    : paloffset! .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET! ;

    \ ( sprite -- paloffset )
    : paloffset@ .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET@ ;

    \ Set the sprite's BPP. 8 or 4.
    \ ( bpp sprite -- )
    : bpp! = 8 swap .attr-addr swap VERA_SPRITE_ATTR_MODEADDR_MODE! ;

    \ Get the sprite's BPP (8 or 4).
    \ ( sprite -- bpp )
    : bpp@ .attr-addr VERA_SPRITE_ATTR_MODEADDR@ if 8 else 4 then ;

    \ Set the sprite's VRAM address
    \ ( addr sprite -- )
    : addr!
      swap VERA-VRAM-BASE - 5 rshift ( sprite addr )
      swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR!
    ;

    \ Get the sprite's VRAM address
    \ ( sprite -- addr )
    : addr@ 
      .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR@ 
      5 lshift VERA-VRAM-BASE +
    ;

    \ Set the bitmap to be used in the sprite object.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-ptr).
    : tile! ( bitmap-descr sprite -- )
      tuck swap >addr ( tileset sprite )
      2dup swap tileset :: bpp> >bpp ( tileset sprite ) 
      2dup swap tileset :: width> >width ( tileset sprite )
      swap tileset :: height> >height ( )
    ;

    \ Retrieve the descriptor of the bitmap containing the sprite's pixel data.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-ptr).
    \ ( sprite -- tile )
    : tile@
      dup .tileset @ ( sprite tileset )
      swap addr> ( tileset addr )
    ;

    \ Commit the sprite object's configration to hardware, 
    \ i.e. to the sprite attribute RAM.
    \ ( sprite -- )
    : commit
      dup .attr-ram-ptr .attr-addr
      2dup @ swap !
      cell+ @ swap cell+ !
    ;
  end-module

  begin-module layer

    : enable ( f layer -- ) if VERA_DC_VIDEO_L1_ENABLE! else VERA_DC_VIDEO_L0_ENABLE! then ;

    : enabled? ( layer -- f ) if VERA_DC_VIDEO_L1_ENABLE@ else VERA_DC_VIDEO_L0_ENABLE@ then 0<> ;

    \ Set tilemap base address for the given layer
    \ ( addr layer -- )
    : (map-base!)
      swap VERA_VRAM_BASE - 9 rshift ( layer vram-base )
      swap if VERA_L1_MAPBASE! else VERA_L0_MAPBASE! then
    ;

      ( layer -- addr )
    : (map-base@)
      if VERA_L1_MAPBASE@ else VERA_L0_MAPBASE@ then
      9 lshift VERA_VRAM_BASE +
    ;

    : (sizeenc) log2 5 - ;

    : (sizedec) 5+ 1<< ;

    \ Set tilemap width for given layer
    \ ( width layer -- )
    : (map-width!)
      swap (sizeenc)
      swap if VERA_L1_CONFIG_MAP_WIDTH! else VERA_L0_CONFIG_MAP_WIDTH! then
    ;

    \ ( layer -- width )
    : (map-width@) if VERA_L1_CONFIG_MAP_WIDTH@ else VERA_L0_CONFIG_MAP_WIDTH@ then (sizedec) ;

      ( height layer -- )
    : (map-height!)
      swap (sizeenc)
      swap if VERA_L1_CONFIG_MAP_HEIGHT! else VERA_L0_CONFIG_MAP_HEIGHT! then
    ;

      ( layer -- height )
    : (map-height@)
      if VERA_L1_CONFIG_MAP_HEIGHT@ else VERA_L0_CONFIG_MAP_HEIGHT@ then sizedec) ;

      ( f layer -- )
    : (t256c!) if VERA_L1_CONFIG_T256C! else VER_L0_CONFIG_T256C! then ;

      ( layer -- f )
    : (t256c@) if VERA_L1_CONFIG_T256C@ else VER_L0_CONFIG_T256C@ then 0<> ;

    \ Configure the given map object into the given layer
    ( map layer -- )
    : tilemap!
      swap ( layer map )
      2dup tilemap :: type@ tilemap :: type :: TXT256 = ( layer map layer t256c )
      swap (t256c)! ( layer map )
      2dup tilemap :: width@ swap (map-width!) ( layer map )
      2dup tilemap :: height@ swap (map-height!) ( layer map )
      tilemap :: base@ swap (map-base!) ( )
    ;

    ( bpp - bpp-encoded )
    : (bppenc) log2 [inline] ;

    ( bpp-encoded -- bpp )
    : (bppdec) 1<< [inline] ;

    ( bpp layer -- )
    : (bpp!)
      swap (bpp-enc) ( layer bpp-encoded )
      swap if VERA_L1_CONFIG_COLORDEPTH! else VERA_L0_CONFIG_COLORDEPTH! then
    ;

    ( layer -- bpp )
    : (bpp@)
      swap if VERA_L1_CONFIG_COLORDEPTH@ else VERA_L0_CONFIG_COLORDEPTH@ then
      (bppdec)
    ;

    ( f layer -- )
    : (bitmap-mode!) if VERA_L1_CONFIG_BITMAP_MODE! else VERA_L0_CONFIG_BITMAP_MODE! then ;

    ( layer -- f )
    : (bitmap-mode@) if VERA_L1_CONFIG_BITMAP_MODE@ else VERA_L0_CONFIG_BITMAP_MODE@ then 0<> ;

    ( paloffset layer -- )
    : (bitmap-paloffset!) if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET! else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then ;

    ( layer -- paloffset )
    : (bitmap-paloffset@) if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET@ else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then ;

    ( hscroll layer -- )
    : (hscroll!)
      dup bitmap-mode> if ( hscroll layer )
        if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( hscroll addr )
        !
      else ( hscroll layer )
        if VERA_L1_HSCROLL_HSCROLL_7_0! else VERA_L0_HSCROLL_HSCROLL_7_0! then
      then
    ;

    ( layer -- hscroll )
    : (hscroll@)
      dup bitmap-mode> if ( layer )
        if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( addr )
        @
      else ( layer )
        if VERA_L1_HSCROLL_HSCROLL_7_0@ else VERA_L0_HSCROLL_HSCROLL_7_0@ then
      then
    ;

    ( hscroll layer -- )
    : (vscroll!) if VERA_L1_VSCROLL_7_0! else VERA_L0_VSCROLL_7_0! then ! ;

    ( layer -- vscroll )
    : (vscroll@) if VERA_L1_VSCROLL_7_0@ else VERA_L0_VSCROLL_7_0@ then ;

    \ In bitmap mode, true sets bitmap width 640, false 320.
    \ In tile mode, true sets tile width 16, false 8.
    \ ( f layer -- )
    : (tile-width!) if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH! else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH! then ;

    ( layer -- f )
    : (tile-width@) if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@ else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@ then 0<> ;

    \ True sets tile height 16, false 8.
    \ ( f layer -- )
    : (tile-height!) if VERA_L1_TILEBASE_TILE_HEIGHT! else VERA_L0_TILEBASE_TILE_HEIGHT! then ;

    ( layer -- f )
    : (tile-height@) if VERA_L1_TILEBASE_TILE_HEIGHT@ else VERA_L0_TILEBASE_TILE_HEIGHT@ then 0<> ;

    ( addr layer -- )
    : (tile-addr!)
      swap VERA_VRAM_BASE - 11 rshift ( layer addr )
      swap if VERA_L1_TILEBASE_TILE_BASEADDR! else VERA_L1_TILEBASE_TILE_BASEADDR! then
    ;

    ( layer -- addr )
    : (tile-addr@)
      if VERA_L1_TILEBASE_TILE_BASEADDR@ else VERA_L1_TILEBASE_TILE_BASEADDR@ then
      11 lshift VERA_VRAM_BASE +
    ;

    \ Configure given tileset into given layer.
    ( tileset layer -- )
    : tileset!
      swap
      2dup tileset :: bpp@ (bpp!) ( layer tileset )
      over false (bitmap-mode!) ( layer tileset )
      2dup tileset :: width@ 16 = (tile-width!) ( layer tileset )
      2dup tileset :: height@ 16 = (tile-height!) ( layer tileset )
      tileset :: base@ (tile-addr!)
    ;

    \ Configure given bitmap (identified by a bitmap descriptor) into the given layer.
    \ A bitmap descriptor is a double consisting of the tuple (tileset bitmap-addr).
    ( bitmap-descr layer -- )
    : bitmap!
      >r ( tileset tileaddr R: layer )
      over tileset :: bpp@ ( tileset tile-addr bpp R: layer )
      r@ swap (bpp!) ( tileset tile-addr R: layer )
      r@ true (bitmap-mode!) ( tileset tile-addr R: layer )
      swap tileset :: width@ 640 = ( tile-addr f R: layer )
      r@ swap (tile-width!) ( tile-addr R: layer )
      r@ 0 (tile-height!) ( tile-addr R: layer )
      r> swap (tile-addr!) ( )
    ;
  end-module

  begin-module line-capture
    ( f -- )
    : enable VERA_CTRL_STATUS_CAPTURE_EN! ;

    ( -- f )
    : enabled? VERA_CTRL_STATUS_CAPTURE_EN@ 0<> ;

    \\  Read the RGB value of a pixel on the captured line.
    \\ @param x: the pixel's x position. Range: 0..639.
    \\ @return: 12-bit RGB triple.
    : pxl@ ( x -- rgb ) 4* VERA_CAPTURE_RAM_BASE + @ $fff and ;
  end-module

  \ --- Interrupt Subsystem ---
  begin-module irq
    VERA_IEN_VAL_VSYNC constant VSYNC_MASK
    VERA_IEN_VAL_LINE constant LINE_MASK
    VERA_IEN_VAL_SPRCOL constant SPRCOL_MASK

    \\ Enable IRQs. The passed in mask will be OR'd with the installed mask.
    \\ @param mask: bitwise OR of VERA_IRQs to enable.
    : enable ( mask -- ) VERA_IEN_ADDR tuck @ or ! ;

    \\ Disable IRQs. The passed in mask will be inverted and  AND'd with the
    \\ installed mask.
    \\ @param mask: bitwise OR of VERA_IRQs to disable.
    : disable ( mask -- ) VERA_IEN_ADDR tuck @ swap bic ! ;

    \\ Retrieve the enabled IRQs bitmask.
    \\ @return: a bitmask of enabled VERA_IRQs.
    : enabled ( -- mask ) VERA_IEN_ADDR @ ;

    \\ Retrieve the active IRQs.
    \\ @return: a bitmask of active VERA_IRQs.
    : get ( -- active-mask ) VERA_ISR_ADDR @ VERA_IEN_ADDR @ and ;

    \\ Acknowledge IRQs.
    \\ @param mask: bitwise OR of VERA_IRQs to acknowledge.
    : ack ( mask -- ) VERA_ISR_ISR! ;

    \\ Set/Get the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
    \\ enabled.
    \\ @param scanline: scanline number on which the trigger the line IRQ, must be
    \\ <= VERA_SCANLINE_MAX.
    : irqline-set ( scanline -- ) VERA_IRQLINE! ;
    : irqline-get ( -- scanline ) VERA_IRQLINE@ ;
    : scanline-get ( -- scanline ) VERA_SCANLINE@ ;
  end-module

  \ --- Palette API ---

  begin-module palette
    \ --- Color Palette Indexes ---
    #0 constant BLACK
    #1 constant WHITE
    #2 constant RED
    #3 constant CYAN
    #4 constant PURPLE
    #5 constant GREEN
    #6 constant BLUE
    #7 constant YELLOW
    #8 constant ORANGE
    #9 constant BROWN
    #10 constant LIGHT_RED
    #11 constant DARK_GREY
    #12 constant GREY
    #13 constant LIGHT_GREEN
    #14 constant LIGHT_BLUE
    #15 constant LIGHT_GREY
    #16 constant GRAYSCALE_0 
    #31 constant GRAYSCALE_15 

    \ Mask given value to 0-15 range and
    \ return corresponding grayscale value in the default VERA color palette.
    \ ( n -- n' )
    : grayscale #15 and GRAYSCALE_0 + [1-foldable] ;

    \ Expects standard 4-bit color fields mapped linearly
    \ Write an entry into the palette.
    \ @param idx: the palete color index
    \ @param rgb: the 12-bit RGB triple
    : write ( idx rgb -- )
      $fff and ( idx rgb )
      swap ( rgb idx )
      4* VERA_PALETTE_RAM_BASE + !
    ;

    \ Read the RGB value of a palette entry
    \ @param idx: the palete color index:
    \ @return: the 12-bit RGB triple
    : read ( idx -- rgb ) 4* VERA_PALETTE_RAM_BASE + @ ;

  end-module

  \ -- VERA top-level definitions
  begin-module top
    : display-enable ( flag -- ) VERA_DC_VIDEO_OUTPUT_MODE! ;

    : display-enabled? ( -- flag ) VERA_DC_VIDEO_OUTPUT_MODE@ 0<> ;

    : sprites-enable ( flag -- ) VERA_DC_VIDEO_SPR_ENABLE! ;

    : sprites-enabled? ( -- flag ) VERA_DC_VIDEO_SPR_ENABLE@ 0<> ;

    : hscale! ( scale-ufix1-7 -- ) VERA_DC_HSCALE! ;
    : hscale@ ( -- scale-ufix1-7 ) VERA_DC_HSCALE@ ;
    : vscale! ( scale-ufix1-7 -- ) VERA_DC_VSCALE! ;
    : vscale! ( -- scale-ufix1-7 ) VERA_DC_VSCALE@ ;
    : bordercolor! ( pal-idx -- ) VERA_DC_BORDERCOLOR! ;
    : bordercolor@ ( -- pal-idx ) VERA_DC_BORDERCOLOR@ ;

    \ Set screen boundaries
    : boundaries! ( hstart hstop vstart vstop -- ) VERA_DC_VSTOP! VERA_DC_VSTART! VERA_DC_HSTOP! VERA_DC_HSTART! ;

    \ Get screen boundaries
    : boundaries@ ( -- hstart hstop vstart vstop ) VERA_DC_HSTART@ VERA_DC_HSTOP@ VERA_DC_VSTART@ VERA_DC_VSTOP@ ;

    ( f -- )
    : sprite-bank! VERA_CTRL_STATUS_SBNK! ;

    ( -- f )
    : sprite-bank@ VERA_CTRL_STATUS_SBNK@ 0<> ;
  end-module
end-module

