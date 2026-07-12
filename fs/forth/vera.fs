\ BoxLambda Forth
\ VERA Graphics Driver

\ 'position' in the definitions below is a vec2.

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

  \ --- VRAM ---

  begin-module vram

    : x-alloc-failed ." VRAM allocation failed." cr ;

    #2048 constant BLOCK_SZ_BYTES
    BLOCK_SZ_BYTES log2 constant LOG_BLOCK_SZ
    VERA_VRAM_SIZE_BYTES LOG_BLOCK_SZ rshift constant NUM_BLOCKS

    create blocks_ NUM_BLOCKS chars allot

    : reset blocks_ NUM_BLOCKS 0 fill ;

    reset

    \ --- VRAM Allocation Subsystem ---
    \ In the blocks_ array, at offset, attempt to find requested blocks.
    \ Return actual number of blocks found (might be less than requested).
    : (find-free-blocks) ( offset requested -- found )
      >r 0 ( offset found R: requested )
      begin
        \ Stop scanning if requested number is found.
        dup r@ < >r ( offset found R: requested f )
        \ Stop scanning if we reached the end of blocks_.
        over NUM_BLOCKS < >r ( offset found f R: requested f f )
        \ Stop scanning if the block is not free.
        over blocks_ + c@ 0=  ( offset found f R: requested f f ) 
        2r> and and while ( offset found R: requested )
          \ Increment offset and found
          1 dup d+ ( offset+1 found+1 R: requested )
      repeat
      rdrop swap drop ( found )
    ;

    \ Find a chunk of num-blocks consecutive free blocks in the blocks_
    \ array. Return the start index of this chunk.
    \ Raise x-vram-alloc-failed exception if no chunk is found.

    \ ( num-blocks -- block-idx|-1 )
    : (find-free-chunk)
      \ Start scanning from offset 0
      NUM_BLOCKS 0 do ( num-blocks )
        \ Attempt to find num-blocks starting from offset block-idx.
        dup i swap (find-free-blocks) ( num-blocks found-blocks )
        \ If not found, increment offset and loop.
        over >= if ( num-blocks )
          drop i
          unloop exit
        then
      loop

      drop -1
    ;

    ( num-blocks block-idx -- )
    : (allocate-blocks)
      blocks_ + ( num-blocks vram_block_ptr )
      2dup c! ( num-blocks vram_block_ptr )
      1+ swap 1- $ff fill ( )
    ;

    : (find-alloc-blocks) ( num-blocks -- block-idx )
      dup (find-free-chunk) ( num-blocks block-idx )
      dup -1 = triggers x-alloc-failed
      tuck (allocate-blocks) ( block-idx )
    ;

    \ Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
    \ The 'init' Words use this function to allocate their resources.
    \ size-bytes: the number of bytes to allocate.
    \ If successful a 2KB-aligned Pointer to allocated block of memory in VRAM.
    \ In not successful an x-vram-alloc-failed exception is raised.
    : alloc ( size-bytes -- addr )
      dup ?assert \ Size can't be 0.
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

    \ Return the VRAM base address.
    \ ( -- vram-base-addr )
    : base VERA_VRAM_BASE ;

  end-module \ VRAM

  \ --- Tile Map API
  begin-module tilemap

    begin-structure tilemap-struct
      field:  .base
      field:  .position \ transient
      hfield: .width
      hfield: .height
      hfield: .tile-idx \ transient
      cfield: .type
      cfield: .color \ transient
      cfield: .paloffset \ transient
      cfield: .vflip \ transient
      cfield: .hflip \ transient
    end-structure

    \ Initialize the tilemap object. This must be done only once.
    \ ( tilemap -- )
    : init tilemap-struct 0 fill ;

    \ Retrieve map width from the tilemap object.
    \ ( tilemap -- width )
    : width@ .width h@ ;

    \ Retrieve map height from the tilemap object.
    \ ( tilemap -- height )
    : height@ .height h@ ;

    \ Map types
    0 constant TXT16
    1 constant TXT256
    2 constant TILE

    \ Retrieve the map type from the map object
    \ ( tilemap -- type )
    : type@ .type c@ ;

    \ Retrieve tilemap base address in VRAM.
    \ ( tilemap -- addr )
    : base@ .base @ ;

    ( size -- f )
    : (size-is-valid) l{ 32 , 64 , 128 , 256 }l find-in 0<> ;

    ( type -- f )
    : (type-is-valid) l{ TXT16 , TXT256 , TILE }l find-in 0<> ;

    begin-module config
      \ tilemap :: { }set attributes

      \ Set map width in the tilemap object: 32, 64, 128, 256
      \ ( tilemap width -- tilemap )
      : width
        dup (size-is-valid) ?assert
        over .width h! ;

      \ Set map height in the tilemap object: 32, 64, 128, 256
      \ ( tilemap height -- tilemap )
      : height
        dup (size-is-valid) ?assert
        over .height h! ;

      \ Map types
      TXT16 constant TXT16
      TXT256 constant TXT256
      TILE constant TILE

      \ Set the map type : TXT16/TXT256/TILE.
      \ ( tilemap type -- tilemap )
      : type
        dup (type-is-valid) ?assert
        over .type c! ;

      \ (Re)Allocate VRAM for this tilemap to accommodate the width and height
      \ If VRAM was previously allocated for this tilemap,
      \ this VRAM will be released before reallocating VRAM.
      \ Throws x-vram-alloc-failed exception if VRAM allocation failed.
      \ ( tilemap -- )
      : }set
        [: 
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
        ;] compile-or-execute
        config unimport
        [immediate]
      ;
    end-module \ tilemap :: config
  end-module \ tilemap

  \ Create and initialize a tilemap object.
  \ ( "name" -- )
  : <tilemap> create here tilemap :: tilemap-struct allot tilemap :: init ;

  \ Opening bracket for tilemap{ ... }set
  ( tilemap -- tilemap )
  : tilemap{ tilemap :: config import [immediate] ;

  begin-module mapentry

    \ Get the address of the entry at position in given map
    : (position>addr) ( position tilemap -- addr )
      [ tilemap import ]
      \ Calculate 2*(row*width_ + col)
      2dup width@ swap vec2.y * ( position map y*w )
      rot vec2.x + 2* ( map offset )
      swap base@ ( offset base )
      dup ?assert
      +
      [ tilemap unimport ]
    ;

    \ Set mapentry at given position in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( mapentry position tilemap -- )
    : (mapentry!) (position>addr) h! ;

    \ Get mapentry at given in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( position tilemap -- mapentry )
    : (mapentry@) (position>addr) h@ ;

    begin-module config
      \ mapentry :: { }set/get attributes

      ( tilemap bg -- tilemap )
      : bg
        [ tilemap import ]
        over type@ TXT16 = ?assert
        4 lshift ( tilemap bgshifted )
        over .color c@ ( tilemap bshifted oldcolor )
        $f and or      ( tilemap newcolor )
        over .color c! ( tilemap )
        [ tilemap unimport ]
      ;

      ( tilemap fg -- tilemap )
      : fg
        [ tilemap import ]
        over type@ TXT16 = if
          $f and         ( tilemap fgmasked )
          over .color c@ ( tilemap fgmasked oldcolor )
          $f0 and or      ( tilemap newcolor )
        then
        over .color c! ( tilemap )
        [ tilemap unimport ]
      ;

      ( tilemap tile-idx -- tilemap )
      : idx 
        [ tilemap import ]
        over .tile-idx h! 
        [ tilemap unimport ]
      ;

      ( tilemap paloffset -- tilemap )
      : paloffset
        [ tilemap import ]
        over type@ TILE = ?assert
        over .paloffset c!
        [ tilemap unimport ]
      ;

      ( tilemap vflip -- tilemap )
      : vflip
        [ tilemap import ]
        over type@ TILE = ?assert
        over .vflip c!
        [ tilemap unimport ]
      ;

      ( tilemap hflip -- tilemap )
      : hflip
        [ tilemap import ]
        over type@ TILE = ?assert
        over .vflip c!
        [ tilemap unimport ]
      ;

      ( tilemap row col -- tilemap )
      : position
        [ tilemap import ]
        swap vec2 ( tilemap vec2 )
        over .position !
        [ tilemap unimport ]
      ;

      \ Run-time portion of }set 
      : }set ( tilemap -- ) 
        [ tilemap import ]
        dup type@ TILE = if ( tilemap )
          dup .paloffset c@ 12 lshift ( tilemap mapentry )
          over .vflip c@ 1 and 11 lshift or ( tilemap mapentry )
          over .hflip c@ 1 and 10 lshift or ( tilemap mapentry )
          over .tile-idx h@ $3ff and or ( tilemap mapentry )
        else
          dup .color c@ 8 lshift ( tilemap mapentry )
          over .tile-idx h@ ( tilemap mapentry tileidx )
          $ff and or ( tilemap mapentry )
        then
        over .position @ ( tilemap mapentry position )
        rot (mapentry!) ( )
        [ tilemap unimport ]
      ;

      \ Write mapentry using attributes specified in {}mapentry! block.y
      : }set
        [:
          [ tilemap import ]
          dup type@ TILE = if ( tilemap )
            dup .paloffset c@ 12 lshift ( tilemap mapentry )
            over .vflip c@ 1 and 11 lshift or ( tilemap mapentry )
            over .hflip c@ 1 and 10 lshift or ( tilemap mapentry )
            over .tile-idx h@ $3ff and or ( tilemap mapentry )
          else
            dup .color c@ 8 lshift ( tilemap mapentry )
            over .tile-idx h@ ( tilemap mapentry tileidx )
            $ff and or ( tilemap mapentry )
          then
          over .position @ ( tilemap mapentry position )
          rot (mapentry!) ( )
          [ tilemap unimport ]
        ;] compile-or-execute
        config unimport
        [immediate]
      ;

      \ Read from VRAM, mapentry specified by { <row> <col> position }mapentry@ and 
      \ decode it, populating fg, bg, paloffset, vflip and hflip attributes.
      \ This is useful for mapentry read-modify-write operations.
      : }get
        [:
          [ tilemap import ]
          >r ( R: tilemap )
          r@ .position @ r@ (mapentry@) ( mapentry R: tilemap )
          r@ type@ TILE = if ( mapentry R: tilemap )
            dup 12 rshift r@ .paloffset c! ( mapentry R: tilemap )
            dup $800 and 0= r@ .vflip c! ( mapentry R: tilemap )
            dup $400 and 0= r@ .hflip c! ( mapentry R: tilemap )
            $3ff and r> .tile-idx h! ( )
          else
            dup 8 rshift r@ .color c! ( mapentry R: tilemap )
            $ff and r> .tile-idx h! ( )
          then
          [ tilemap unimport ]
        ;] compile-or-execute
        config unimport
        [immediate]
      ;

    end-module \ mapentry :: config

    \ set a 16-bit mapentry value at row/col in given tilemap
    ( mapentry row col tilemap )
    : set -rot swap vec2 swap tilemap :: (mapentry!) ;

    \ get the 16-bit mapentry value from row/col in given tilemap
    ( row col tilemap -- mapentry )
    : get -rot swap vec2 swap tilemap :: (mapentry@) ;

  end-module \ mapentry

  \ Opening bracket for mapentry{ ... }set and { ... }get
  ( tilemap -- tilemap )
  : mapentry{ mapentry :: config import [immediate] ;

  \ Keeping the 16-bit mapentry unpack words directly in the vera namespace for convenience:

  \ Unpack chr, fg and bg color from a 1bpp 16 color textmode map entry value
  \ ( mapentry -- chr fg bg )
  : unpack-txt16
    dup $ff and ( mapentry chr )
    swap dup 8 rshift $f and ( chr mapentry fg )
    swap 12 rshift $f and ( chr fg bg )
  ;

  \ Unpack chr and fg color from a 1bpp 256 color textmode map entry value
  \ ( mapentry -- chr fg )
  : unpack-txt256
    dup $ff and ( mapentry chr )
    swap 8 rshift $ff and ( chr fg )
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

  \ -- Pixel API
  \ Getting and Setting pixels in tiles:

  begin-module pixel

    ( base width position -- ptr )
    : (8bpp-byte-ptr) vec2.xy rot * + + [inline] ;

    ( pxlval position base width -- )
    : 8bpp! 
      rot (8bpp-byte-ptr) ( pxval ptr )
      c! ( )
    ;

    ( position base width -- pxlval )
    : 8bpp@ 
      rot (8bpp-byte-ptr) ( ptr )
      c@ ( pxlval )
    ;

    ( base width position -- ptr )
    : (4bpp-byte-ptr) vec2.xy rot * + 2/ + [inline] ;

    ( position -- bitoffset )
    : (4bpp-x-bitoffset)
      vec2.x
      1 dup rot \ 1 1 x
      and - \ 1-x&1
      2 lshift [inline] ;

    ( pxlval position base width -- )
    : 4bpp! 
      rot dup (4bpp-x-bitoffset) >r ( pxlval base y width position R: bitoffset )
      (4bpp-byte-ptr) ( pxval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      $f r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
      rot r> lshift or ( ptr newbyte )
      swap c! ( )
    ;

    ( position base width -- pxlval )
    : 4bpp@
      rot dup (4bpp-x-bitoffset) >r ( base width position R: bitoffset )
      (4bpp-byte-ptr) ( ptr R: bitoffset )
      c@ ( oldbyte R: bitoffset )
      r> rshift $f and ( pxlval )
    ;

    ( base width position -- ptr )
    : (2bpp-byte-ptr) vec2.xy rot * + 4/ + [inline] ;

    ( position -- bitoffset )
    : (2bpp-x-bitoffset)
      vec2.x
      3 dup rot \ 3 3 x 
      and - \ 3-x&3 
      shl \ (3-x&3)*2
    ;

    ( pxlval position base width -- )
    : 2bpp!
      rot dup (2bpp-x-bitoffset) >r ( pxlval base width position R: bitoffset )
      (2bpp-byte-ptr) ( pxval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      3 r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
      rot r> lshift or ( ptr newbyte )
      swap c! ( )
    ;

    ( position base width -- pxlval )
    : 2bpp@
      rot dup (2bpp-x-bitoffset) >r ( base width position R: bitoffset )
      (2bpp-byte-ptr) ( ptr R: bitoffset )
      c@ ( oldbyte R: bitoffset )
      r> rshift 3 and ( pxlval )
    ;

    ( base width position -- ptr )
    : (1bpp-byte-ptr) vec2.xy rot * + 8/ + [inline] ;

    ( position -- bitoffset )
    : (1bpp-x-bitoffset) vec2.x 7 dup rot ( 7 7 x ) and - ( 7-x&7 ) [inline] ;

    ( pxlval position base width -- )
    : 1bpp!
      rot dup (1bpp-x-bitoffset) >r ( pxlval base width position R: bitoffset )
      (1bpp-byte-ptr) ( pxlval ptr R: bitoffset )
      dup c@ ( pxlval ptr oldbyte R: bitoffset )
      rot r> setbit ( ptr newbyte )
      swap c! ( )
    ;

    ( position base width -- pxlval )
    : 1bpp@
      rot dup (1bpp-x-bitoffset) >r ( base width position R: bitoffset )
      (1bpp-byte-ptr) ( ptr R: bitoffset )
      c@ r> rshift 1 and ( pxlval )
    ;
  end-module \ pixel

  \ -- Tileset API
  \ A tileset is used to represent tiles, sprite pixel data and bitmaps.

  begin-module tileset

    begin-structure tileset-struct
      field:  .base
      field:  .pxl-set
      field:  .pxl-get
      field:  .position \ transient
      field:  .bitmapaddr \ transient
      hfield: .width
      hfield: .height
      hfield: .bpp
      hfield: .num-tiles
      hfield: .color
    end-structure

    \ Initialize the tileset object. This must be done only once.
    : init ( tileset -- ) tileset-struct 0 fill ;

    \ Retrieve the tileset width from the tileset object
    ( tileset -- width )
    : width@ .width h@ ;

    \ Retrieve tileset base address in VRAM.
    ( tileset -- addr )
    : base@ .base @ ;

    \ Retrieve the tileset height
    ( tileset -- height )
    : height@ .height h@ ;

    \ Retrieve the tileset BPP from the tileset object.
    ( tileset -- bpp )
    : bpp@ .bpp h@ ;

    ( tileset -- num-tiles )
    : num-tiles@ .num-tiles h@ ;

    \ Retrieve the tilesize in bytes for the given tileset.
    ( tileset -- tilesize-bytes )
    : tilesize@ dup bpp@ swap dup width@ swap height@ * * 8/ ;

    \ Given a VRAM address and a tileset, compute the tile index corresponding to that address.
    ( addr tileset -- tile-idx )
   : addr>tile-idx
     dup base@ dup ?assert ( addr tileset baseaddr )
     rot swap - ( tileset offset )
     swap tilesize@ ( offset tilesize )
     / ( tileidx )
   ;

    \ Given a tile index in a tileset, compute to  pointer to the pixel data of a tile in the tileset.
    \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
    \ @param tileset: Tileset object
    ( tile-idx tileset -- addr )
    : tile-idx>addr
      dup tilesize@ rot * ( tileset tilesize*tile-idx ) 
      swap base@ dup ?assert
      + ;

    begin-module config

      \ Set the tileset width in the tileset object.
      \   - 8, 16 for regular tiles.
      \   - 8, 16, 32, 64 for sprites.
      \   - 320, 640 for bitmaps.
      ( tileset width -- tileset )
      : width
        over .width h! 
      ;

      \ Set the tileset height in the tileset object
      \   - 8 or 16 for regular tiles.
      \   - 8, 16, 32, 64 for sprites.
      \   - Any positive value for bitmaps.
      ( tileset height -- tileset )
      : height over .height h! ;

      \ Set the tileset BPP in the tileset object
      \   - 1, 2, 4, 8 for regular tiles and bitmaps.
      \   - 4, 8 for sprites.
      ( tileset bpp -- tileset )
      : bpp
        swap >r ( bpp R: tileset )
        dup r@ .bpp h! ( bpp R: tileset )
        [ pixel import ]
        case
          1 of ['] 1bpp! ['] 1bpp@ endof
          2 of ['] 2bpp! ['] 2bpp@ endof
          4 of ['] 4bpp! ['] 4bpp@ endof
          8 of ['] 8bpp! ['] 8bpp@ endof
          false ?assert
        endcase ( setter getter R: tileset )
        [ pixel unimport ]
        r@ .pxl-get ! ( R: tileset )
        r@ .pxl-set ! ( R: tileset )
        r> ( tileset )
      ;

      \ Set the number of tiles in the tileset.
      \ Range: 0..1023
      ( tileset num -- tileset )
      : tiles
        dup 1024 < ?assert ( tileset num )
        over .num-tiles h! 
      ;

      \ (Re)Allocate VRAM for this tileset to accommodate
      \ num-tiles, bpp, width and height.
      \ If VRAM was previously allocated for this tileset,
      \ this VRAM will be released before reallocating VRAM.
      \ Throws x-vram-alloc-failed exception if VRAM allocation failed.
      ( tileset -- )
      : }set
        [:
          >r ( R: tileset )
          r@ base@ ?dup if
            vram :: free
          then ( R: tileset )
          0 r@ .base ! ( R: tileset )
          r@ tilesize@ r@ num-tiles@ * 
          vram :: alloc ( addr R: tileset )
          r> .base ! ( R: tileset )
        ;] compile-or-execute
        config unimport
        [immediate]
      ;
    end-module \ tileset :: config

  end-module \ tileset

  \ Create and initialize a tileset object.
  \ ( "name" -- )
  : <tileset> create here tileset :: tileset-struct allot tileset :: init ;

  \ Opening bracket for tileset{ ... }set
  ( tileset -- tileset )
  : tileset{ tileset :: config import [immediate] ;

  pixel continue-module
    begin-module config

      \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
      \ @param tileset: Tileset object
      ( tileset tile-idx -- tileset )
      : tile 
        2dup swap tileset :: num-tiles@ <= ?assert ( tileset tile-idx )
        over tileset :: tile-idx>addr ( tileset addr )
        over tileset :: .bitmapaddr ! ( tileset )
      ;

      ( tileset color -- tileset )
      : color
        over tileset :: .color h!
      ;

      ( tileset x y -- tileset )
      : position vec2 over tileset :: .position ! ;

      \ Set a pixel in the given tile.
      ( tileset -- )
      : }set
        [:
          >r ( R: tileset )
          r@ tileset :: .color h@
          r@ tileset :: .position @
          r@ tileset :: .bitmapaddr @
          r@ tileset :: .width h@ 
          r> tileset :: .pxl-set @
          ( color position addr width pxl-setter )
        execute
        ;] compile-or-execute
        config unimport
        [immediate]
      ;

      \ Read the pixel color from the given position on the given tile.
      \ ( tileset -- color )
      : }get
        [:
          >r ( R: tileset )
          r@ tileset :: .position @
          r@ tileset :: .bitmapaddr @
          r@ tileset :: .width h@
          r@ tileset :: .pxl-get @
          ( position addr width pxl-getter R: tileset )
          execute ( color R: tileset )
          dup r> tileset :: .color ! ( color )
        ;] compile-or-execute
        config unimport
        [immediate]
      ;
    end-module \ pixel :: config

  end-module \ pixel

  \ Opening bracket for pixel{ ... }set/get
  ( tileset -- tileset )
  : pixel{ pixel :: config import [immediate] ;

  \ -- Sprite API

  begin-module sprite
    begin-structure sprite-struct
      field:  .tileset
      field:  .tile-idx
      field:  .attr-ram-ptr
      hfield: .attr-addr
      hfield: .attr-x
      hfield: .attr-y
      hfield: .attr-flags
    end-structure

    \ Calculate the sprite attribute RAM address from the given sprite id.
    \ ( id -- addr )
    : (id>ram) 8 * VERA_SPRITE_RAM_BASE + [inline] ;

    \ Calculate the sprite id from the given sprite attribute RAM address.
    \ ( addr -- id )
    : (ram>id) VERA_SPRITE_RAM_BASE - 8 / [inline] ;

    : init ( sprite-idx sprite -- )
      over NUM_SPRITES u< ?assert
      dup sprite-struct 0 fill ( sprite-idx sprite )
      swap (id>ram) ( sprite ramaddr )
      swap .attr-ram-ptr ! ( sprite )
    ;

    \ Retrieve the sprite id from the sprite object.
    : id@ ( sprite -- id ) .attr-ram-ptr @ (ram>id) ;

    \ Get the sprite's current coordinates.
    \ ( sprite -- x y )
    : position@ dup .attr-x h@ swap .attr-y h@ ;

    \ ( tilesize - tilesize-encoded )
    : (sizeenc) log2 3 - ;

    \ ( tilesize-encoded -- tilesize )
    : (sizedec) 3 + 1<< ;

    \ Get the sprite width
    \ ( sprite -- width )
    : width@ .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH@ (sizedec) ;

    \ Get the sprite height
    \ ( sprite -- height )
    : height@
      .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT@ (sizedec) ;

    \ Set the sprite width
    \ ( width sprite -- )
    : (width!) swap (sizeenc) swap .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH! ;

    \ Set the sprite height
    \ ( height sprite -- )
    : (height!)
      swap (sizeenc) swap .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT! ;

    \ ( sprite -- f )
    : vflip@ .attr-flags VERA_SPRITE_ATTR_FLAGS_VFLIP@ 0<> ;

    \ ( sprite -- f )
    : hflip@ .attr-flags VERA_SPRITE_ATTR_FLAGS_HFLIP@ 0<> ;

    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS constant DIS \ Sprite disabled. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 constant BG_L0 \ Between background and L0. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 constant L0_L1 \ Between L0 and L1. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 constant L1 \ In front of L1. 

    \ ( sprite -- f )
    : zdepth@ .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH@ ;

    \ ( sprite -- colmask )
    : colmask@ .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK@ ;

    \ ( sprite -- paloffset )
    : paloffset@ .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET@ ;

    \ Set the sprite's BPP. 8 or 4.
    \ ( bpp sprite -- )
    : (bpp!) swap 8 = swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE! ;

    \ Get the sprite's BPP (8 or 4).
    \ ( sprite -- bpp )
    : bpp@ .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE@ if 8 else 4 then ;

    \ Set the sprite's VRAM address
    \ ( addr sprite -- )
    : (addr!)
      swap VERA_VRAM_BASE - 5 rshift ( sprite addr )
      swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR!
    ;

    \ Get the sprite's VRAM address
    \ ( sprite -- addr )
    : addr@ 
      .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR@ 
      5 lshift VERA_VRAM_BASE +
    ;

    \ Retrieve the tileset corresponding to this sprite.
    \ ( sprite -- tileset )
    : tset@ .tileset @ [inline] ;

    \ Retrieve the tile-idx corresponding to this sprite.
    \ ( sprite -- tile-idx )
    : tidx@ .tile-idx @ [inline] ;

    begin-module config
      \ ( sprite x y -- sprite )
      : position
          rot >r ( x y R: sprite )
          r@ .attr-y h!
          r@ .attr-x h!
          r> ( sprite )
      ;

      \ ( sprite vflip -- sprite )
      : vflip over .attr-flags VERA_SPRITE_ATTR_FLAGS_VFLIP! ;

      \ ( sprite hflip -- sprite )
      : hflip over .attr-flags VERA_SPRITE_ATTR_FLAGS_HFLIP! ;

      \ ( sprite zdepth -- sprite )
      : zdepth over .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH! ;

      \ ( sprite colmask -- sprite )
      : colmask over .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK! ;

      \ ( sprite paloffset -- sprite )
      : paloffset over .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET! ;

      \ Set the tile index to be used in the sprite object.
      \ ( sprite tile-idx -- sprite )
      : tile-idx
        2dup swap .tile-idx ! ( sprite tile-idx )
        \ Compute and set the address attribute if we have a tileset.
        \ If we don't have a tileset yet, this is deferred until the tileset
        \ is specified.
        over .tileset @ ?dup if ( sprite tile-idx tileset )
          tileset :: tile-idx>addr ( sprite addr ) 
          over (addr!) ( sprite )
        else
          drop ( sprite )
        then
      ;
  
      \ Set the tileset to be used in the sprite object.
      \ When modifying the tileset used by a sprite object, keep in mind that
      \ the corresponding tile index (tidx, see above) has to be valid (within
      \ range) for the new tileset.
      : tileset ( sprite tileset -- sprite )
        swap >r ( tileset R : sprite )
        r@ .tile-idx @ ( tileset tile-idx R: sprite )
        over tileset :: tile-idx>addr r@ (addr!) ( tileset R: sprite )
        dup tileset :: bpp@ r@ (bpp!) ( tileset R: sprite )
        dup tileset :: width@ r@ (width!) ( tileset R: sprite )
        dup tileset :: height@ r@ (height!) ( tileset R: sprite )
        r@ .tileset ! ( R: sprite )
        r>
      ;

      \ Commit the sprite's attributes to hardware, 
      \ i.e. to the sprite attribute RAM.
      \ ( sprite -- )
      : }set
        [:
          dup .attr-ram-ptr @ ( sprite attr-ram-addr )
          dup ?assert ( sprite attr-ram-addr )
          swap .attr-addr ( attr-ram-addr attr-addr )
          2dup @ ( attr-ram-addr attr-addr attr-ram-addr attr0/1 ) 
          swap ! ( attr-ram-addr attr-addr )
          cell+ @ ( attr-ram-addr attr1/2 )
          swap cell+ ( attr1/2 attr-ram-addr' )
          !
        ;] compile-or-execute
        config unimport
        [immediate]
      ;
    end-module \ sprite :: config

  end-module

  \ Create and initialize a sprite object.
  \ sprite-idx must be in range 0..NUM_SPRITES-1.
  \ ( sprite-idx "name" -- )
  : <sprite> 
    create here sprite :: sprite-struct allot ( sprite-idx sprite )
    sprite :: init ;

  \ Opening bracket for sprite{ ... }set
  ( sprite -- sprite )
  : sprite{ sprite :: config import [immediate] ;

  begin-module layer

    begin-structure layer-struct
      field:  .tileset
      field:  .tilemap
      hfield: .tile-idx
      cfield: .id
    end-structure

    \ Initialize a layer object
    : init ( id layer -- )
      over NUM_LAYERS < ?assert
      dup layer-struct 0 fill
      .id c!
    ;

    : enable ( f layer -- ) .id c@ if VERA_DC_VIDEO_L1_ENABLE! else VERA_DC_VIDEO_L0_ENABLE! then ;

    : enabled? ( layer -- f ) .id c@ if VERA_DC_VIDEO_L1_ENABLE@ else VERA_DC_VIDEO_L0_ENABLE@ then 0<> ;

    \ Set tilemap base address for the given layer
    \ ( addr layer-id -- )
    : (tilemap-base!)
      swap VERA_VRAM_BASE - 9 rshift ( layer-id vram-base )
      swap if VERA_L1_MAPBASE! else VERA_L0_MAPBASE! then
    ;

      ( layer -- addr )
    : tilemap-base@
      .id c@
      if VERA_L1_MAPBASE@ else VERA_L0_MAPBASE@ then
      9 lshift VERA_VRAM_BASE +
    ;

    : (sizeenc) log2 5 - [inline] ;

    : (sizedec) 5 + 1<< [inline] ;

    \ Set tilemap width for given layer
    \ ( width layer-id -- )
    : (tilemap-width!)
      swap (sizeenc)
      swap if VERA_L1_CONFIG_MAP_WIDTH! else VERA_L0_CONFIG_MAP_WIDTH! then
    ;

    \ ( layer -- width )
    : tilemap-width@ .id c@ if VERA_L1_CONFIG_MAP_WIDTH@ else VERA_L0_CONFIG_MAP_WIDTH@ then (sizedec) ;

      ( height layer-id -- )
    : (tilemap-height!)
      swap (sizeenc)
      swap if VERA_L1_CONFIG_MAP_HEIGHT! else VERA_L0_CONFIG_MAP_HEIGHT! then
    ;

      ( layer-id -- height )
    : tilemap-height@
      .id c@
      if VERA_L1_CONFIG_MAP_HEIGHT@ else VERA_L0_CONFIG_MAP_HEIGHT@ then (sizedec) ;

      ( f layer-id -- )
    : (t256c!) if VERA_L1_CONFIG_T256C! else VERA_L0_CONFIG_T256C! then ;

      ( layer -- f )
    : t256c@ .id c@ if VERA_L1_CONFIG_T256C@ else VERA_L0_CONFIG_T256C@ then 0<> ;

    ( bpp - bpp-encoded )
    : (bppenc) log2 [inline] ;

    ( bpp-encoded -- bpp )
    : (bppdec) 1<< [inline] ;

    ( bpp layer-id -- )
    : (bpp!)
      swap (bppenc) ( layer bpp-encoded )
      swap if VERA_L1_CONFIG_COLORDEPTH! else VERA_L0_CONFIG_COLORDEPTH! then
    ;

    ( layer -- bpp )
    : bpp@
      .id c@
      swap if VERA_L1_CONFIG_COLORDEPTH@ else VERA_L0_CONFIG_COLORDEPTH@ then
      (bppdec)
    ;

    ( f layer-id -- )
    : (bitmap-mode!) if VERA_L1_CONFIG_BITMAPMODE! else VERA_L0_CONFIG_BITMAPMODE! then ;

    ( layer -- f )
    : bitmap-mode@ .id c@ if VERA_L1_CONFIG_BITMAPMODE@ else VERA_L0_CONFIG_BITMAPMODE@ then 0<> ;

    ( paloffset layer-id -- )
    : (paloffset!) if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET! else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then ;

    ( layer -- paloffset )
    : paloffset@ .id c@ if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET@ else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then ;

    ( hscroll layer -- )
    : hscroll!
      dup .id c@ ( hscroll layer id )
      swap bitmap-mode@ if ( hscroll id )
        if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( hscroll addr )
        !
      else ( hscroll layer id )
        if VERA_L1_HSCROLL_HSCROLL_7_0! else VERA_L0_HSCROLL_HSCROLL_7_0! then
      then
    ;

    ( layer -- hscroll )
    : hscroll@
      dup .id c@ ( layer id )
      swap bitmap-mode@ if ( id )
        if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( addr )
        @
      else ( id )
        if VERA_L1_HSCROLL_HSCROLL_7_0@ else VERA_L0_HSCROLL_HSCROLL_7_0@ then
      then
    ;

    ( hscroll layer -- )
    : vscroll! 
      .id c@ if VERA_L1_VSCROLL! else VERA_L0_VSCROLL! then ! ;

    ( layer -- vscroll )
    : vscroll@ .id c@ if VERA_L1_VSCROLL@ else VERA_L0_VSCROLL@ then ;

    \ In bitmap mode, true sets bitmap width 640, false 320.
    \ In tile mode, true sets tile width 16, false 8.
    \ ( f layer-id -- )
    : (tile-width!) if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH! else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH! then ;

    ( layer -- f )
    : tile-width@ .id c@ if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@ else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@ then 0<> ;

    \ True sets tile height 16, false 8.
    \ ( f layer-id -- )
    : (tile-height!) if VERA_L1_TILEBASE_TILE_HEIGHT! else VERA_L0_TILEBASE_TILE_HEIGHT! then ;

    ( layer -- f )
    : tile-height@ .id c@ if VERA_L1_TILEBASE_TILE_HEIGHT@ else VERA_L0_TILEBASE_TILE_HEIGHT@ then 0<> ;

    ( addr layer-id -- )
    : (tile-base!)
      swap VERA_VRAM_BASE - 11 rshift ( layer addr )
      swap if VERA_L1_TILEBASE_TILE_BASEADDR! else VERA_L0_TILEBASE_TILE_BASEADDR! then
    ;

    ( layer -- addr-id )
    : tile-base@
      .id c@
      if VERA_L1_TILEBASE_TILE_BASEADDR@ else VERA_L0_TILEBASE_TILE_BASEADDR@ then
      11 lshift VERA_VRAM_BASE +
    ;

    \ Configure given tileset into given layer.
    ( layer-id tileset -- )
    : (tileset!)
      [ tileset import ]
      2dup bpp@ (bpp!) ( layer-id tileset )
      over false (bitmap-mode!) ( layer-id tileset )
      2dup width@ 16 = (tile-width!) ( layer-id tileset )
      2dup height@ 16 = (tile-height!) ( layer-id tileset )
      base@ dup ?assert
      (tile-base!)
      [ tileset unimport ]
    ;

    \ Configure given bitmap (identified by a bitmap descriptor) into the given layer.
    ( tileset tile-idx layer-id -- )
    : (bitmap!)
      [ tileset import ]
      >r ( tileset tile-idx R: layer )
      over tile-idx>addr ( tileset tile-addr R: layer )
      over bpp@ ( tileset tile-addr bpp R: layer )
      r@ swap (bpp!) ( tileset tile-addr R: layer )
      r@ true (bitmap-mode!) ( tileset tile-addr R: layer )
      swap width@ 640 = ( tile-addr f R: layer )
      r@ swap (tile-width!) ( tile-addr R: layer )
      r@ 0 (tile-height!) ( tile-addr R: layer )
      r> swap (tile-base!) ( )
      [ tileset unimport ]
    ;

    \ Retrieve tileset used by this layer
    ( layer -- tileset )
    : tileset@ .tileset @ ;

    \ Retrieve tile-idx used by this layer (bitmap mode).
    ( layer -- tileset )
    : tile-idx@ .tile-idx h@ ;

    \ Retrieve tilemap used by this layer (tilemap mode).
    ( layer -- tilemap )
    : tilemap@ .tileset @ ;

    begin-module config
      \ Set the tilemap to be used by this layer (configuring tilemapmode)
      ( layer map -- layer )
      : tilemap over .tilemap ! ;

      \ Set the tileset to be used by this layer (tilemapmode and bitmapmode)
      ( layer tileset -- layer )
      : tileset over .tileset ! ;
  
      \ Set the tile index to be used by this layer (bitmapmode)
      ( layer tile-idx -- layer )
      : tile-idx over .tile-idx h! ;

      \ Configure the layer in tilemap mode. tmap and tset must be specified.
      ( layer -- )
      : }tilemap-mode
        [:
          [ vera :: tilemap import ]
          dup .id c@ ( layer layer-id )
          over .tilemap @ ( layer layer-id map )
          dup ?assert
          2dup type@ TXT256 = ( layer layer-id map layer-id t256c )
          swap (t256c!) ( layer layer-id map )
          2dup width@ swap (tilemap-width!) ( layer layer-id map )
          2dup height@ swap (tilemap-height!) ( layer layer-id map )
          base@ dup ?assert ( layer layer-id base )
          over (tilemap-base!) ( layer layer-id )
          [ vera :: tilemap unimport ]
          swap .tileset @ ( layer-id tileset )
          dup ?assert
          (tileset!)
        ;] compile-or-execute
        config unimport
        [immediate]
      ;

      \ Configure the layer in bitmap mode. tset and tidx must be specified.
      ( layer -- )
      : }bitmap-mode
        [: 
          dup .id c@ ( layer layer-id )
          over .tileset @ ( layer layer-id tileset )
          dup ?assert
          rot .tile-idx h@ ( layer-id tileset tile-idx )
          (bitmap!)
        ;] compile-or-execute
        config unimport
        [immediate]
      ;
    end-module \ layer :: config

  end-module \ layer

  \ l0 and l1 are the objects to be passed into the public words below.
  create l0 layer :: layer-struct allot
  create l1 layer :: layer-struct allot
  0 l0 layer :: init
  1 l1 layer :: init

  \ opening brack for layer{ ... }tilemap-mode or layer :: { ... }bitmap-mode
  ( layer -- layer )
  : layer{ layer :: config import [immediate] ;

  begin-module line-capture
    ( f -- )
    : enable VERA_CTRL_STATUS_CAPTURE_EN! ;

    ( -- f )
    : enabled? VERA_CTRL_STATUS_CAPTURE_EN@ 0<> ;

    \  Read the RGB value of a pixel on the captured line.
    \ @param x: the pixel's x position. Range: 0..639.
    \ @return: 12-bit RGB triple.
    : pxl@ ( x -- rgb ) 4 * VERA_CAPTURE_RAM_BASE + @ $fff and ;
  end-module

  \ --- Interrupt Subsystem ---
  begin-module irq
    VERA_IEN_VAL_VSYNC constant VSYNC_MASK
    VERA_IEN_VAL_LINE constant LINE_MASK
    VERA_IEN_VAL_SPRCOL constant SPRCOL_MASK

    \ Enable IRQs. The passed in mask will be OR'd with the installed mask.
    \ @param mask: bitwise OR of VERA_IRQs to enable.
    : enable ( mask -- ) VERA_IEN_ADDR tuck @ or ! ;

    \ Disable IRQs. The passed in mask will be inverted and  AND'd with the
    \ installed mask.
    \ @param mask: bitwise OR of VERA_IRQs to disable.
    : disable ( mask -- ) VERA_IEN_ADDR tuck @ swap bic ! ;

    \ Retrieve the enabled IRQs bitmask.
    \ @return: a bitmask of enabled VERA_IRQs.
    : enabled ( -- mask ) VERA_IEN_ADDR @ ;

    \ Retrieve the active IRQs.
    \ @return: a bitmask of active VERA_IRQs.
    : get ( -- active-mask ) VERA_ISR_ADDR @ VERA_IEN_ADDR @ and ;

    \ Acknowledge IRQs.
    \ @param mask: bitwise OR of VERA_IRQs to acknowledge.
    : ack ( mask -- ) VERA_ISR_ISR! ;

    \ Set/Get the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
    \ enabled.
    \ @param scanline: scanline number on which the trigger the line IRQ, must be
    \ <= VERA_SCANLINE_MAX.
    : irqline-set ( scanline -- ) VERA_IRQLINE! ;
    : irqline-get ( -- scanline ) VERA_IRQLINE@ ;
    : scanline-get ( -- scanline ) VERA_SCANLINE@ ;
  end-module

  \ --- Palette API

  \ Color Palette Indices
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

  begin-module palette

    \ Expects standard 4-bit color fields mapped linearly
    \ Write an entry into the palette.
    \ @param idx: the palete color index
    \ @param rgb: the 12-bit RGB triple
    : write ( rgb idx -- )
      swap ( idx rgb )
      $fff and ( idx rgbmasked )
      swap ( rgbmasked idx )
      4 * VERA_PALETTE_RAM_BASE + !
    ;

    \ Read the RGB value of a palette entry
    \ @param idx: the palete color index:
    \ @return: the 12-bit RGB triple
    : read ( idx -- rgb ) 4 * VERA_PALETTE_RAM_BASE + @ ;

  end-module

  \ -- VERA top-level definitions
  : display-enable ( flag -- ) VERA_DC_VIDEO_OUTPUT_MODE! ;

  : display-enabled? ( -- flag ) VERA_DC_VIDEO_OUTPUT_MODE@ 0<> ;

  : sprites-enable ( flag -- ) VERA_DC_VIDEO_SPR_ENABLE! ;

  : sprites-enabled? ( -- flag ) VERA_DC_VIDEO_SPR_ENABLE@ 0<> ;

  : hscale! ( scale-ufix1-7 -- ) VERA_DC_HSCALE! ;
  : hscale@ ( -- scale-ufix1-7 ) VERA_DC_HSCALE@ ;
  : vscale! ( scale-ufix1-7 -- ) VERA_DC_VSCALE! ;
  : vscale@ ( -- scale-ufix1-7 ) VERA_DC_VSCALE@ ;
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

