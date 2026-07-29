\ BoxLambda Forth
\ VERA Graphics Driver

begin-module vera

  \ --- System Limits and Enumerations ---
  #479 constant SCANLINE_VISIBLE_MAX
  #524 constant SCANLINE_MAX
  #1023 constant HSTOP_MAX
  #1023 constant VSTOP_MAX
  #1024 constant MAX_TILES_IN_TILESET
  #2 constant #LAYERS
  #2 constant #SPRITE_BANKS
  #64 constant #SPRITES_IN_BANK
  #SPRITE_BANKS #SPRITES_IN_BANK * constant #SPRITES
  #127 constant MAX_SPRITE_ID

  \ For setting the flip attribute of mapentries and sprites
  2 constant VFLIP
  1 constant HFLIP
  3 constant VFLIP_HFLIP

  : (flip-is-valid?) l{ VFLIP , HFLIP , VFLIP_HFLIP }l find-in 0<> ;

  \ --- VRAM ---

  begin-module vram

    : x-alloc-failed ." VRAM allocation failed." cr ;

    #2048 constant BLOCK-SZ-BYTES
    BLOCK-SZ-BYTES log2 constant LOG-BLK-SZ
    VERA_VRAM_SIZE_BYTES LOG-BLK-SZ rshift constant #BLOCKS

    create blocks_ #BLOCKS chars allot

    : reset blocks_ #BLOCKS 0 fill ;

    reset

    \ --- VRAM Allocation Subsystem ---
    \ In the blocks_ array, at offset, attempt to find requested blocks.
    \ Return actual # of blocks found (might be less than requested).
    : find-free-blocks ( requested offset -- found )
      begin ( left offset )
        over 0> \ Anything left to find? ( left offset f )
        over #BLOCKS < and \ offset < end of blocks_? ( left offset f )
        over blocks_ + c@ 0= and \ blocks_[offset] free? ( left offset f )
        while \ While all of the above are true, keep going. ( left offset )
          1+ swap 1- swap \ Increment offset and decrement left ( left offset )
      repeat
      drop ( left )
    ;

    \ Find a chunk of #blocks consecutive free blocks in the blocks_
    \ array. Return the start index of this chunk.
    \ Raise x-vram-alloc-failed exception if no chunk is found.
    \ ( #blocks -- block-idx|-1 )
    : find-free-chunk
      #BLOCKS 0 do \ Start scanning from offset 0 ( #blocks )
        \ Attempt to find #blocks starting from offset i.
        dup i find-free-blocks ( #blocks remaining-blocks )
        0= if \ If no remaining-blocks, we're done. ( #blocks )
          drop i
          unloop exit
        then
      loop
      drop -1
    ;

    ( #blocks block-idx -- )
    : allocate-blocks
      blocks_ + ( #blocks vram_block_ptr )
      2dup c! ( #blocks vram_block_ptr )
      1+ swap 1- $ff fill ( )
    ;

    : find-alloc-blocks ( #blocks -- block-idx )
      dup find-free-chunk ( #blocks block-idx )
      dup -1 = triggers x-alloc-failed
      tuck allocate-blocks ( block-idx )
    ;

    \ ( block-idx -- )
    : free
      blocks_ + ( vram_block_ptr )
      dup c@ ( vram_block_ptr #blocks )
      0 fill ( )
    ;
  end-module \ VRAM

  : vram-reset vram :: reset ;

  \ Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
  \ The 'init' Words use this function to allocate their resources.
  \ size-bytes: the number of bytes to allocate.
  \ If successful a 2KB-aligned Pointer to allocated block of memory in VRAM.
  \ In not successful an x-vram-alloc-failed exception is raised.
  : vram-alloc ( size-bytes -- addr )
    [: dup ;] xassert \ Size can't be 0.
    \ Convert size in bytes to block size, rounding up.
    [ vram :: BLOCK-SZ-BYTES 1- ] literal + vram :: LOG-BLK-SZ rshift ( #blocks )
    vram :: find-alloc-blocks ( block-idx )
    \ Convert to address
    vram :: LOG-BLK-SZ lshift VERA_VRAM_BASE +
  ;

  \ Release VRAM allocated with vram-alloc.
  : vram-free ( addr -- )
    \ Convert addr to block-idx
    VERA_VRAM_BASE - vram :: LOG-BLK-SZ rshift ( block-idx )
    vram :: free
  ;

  \ Return the VRAM base address.
  \ ( -- vram-base-addr )
  : vram-base VERA_VRAM_BASE ;

  \ --- Tile Map API

  \ Map types
  0 constant TMAP-TXT16
  1 constant TMAP-TXT256
  2 constant TMAP-TILE

  ( type -- f )
  : (tmap-type-is-valid?) l{ TMAP-TXT16 , TMAP-TXT256 , TMAP-TILE }l find-in 0<> ;

  begin-module tilemap

    begin-structure tilemap-struct
      field:  .base
      hfield: .width
      hfield: .height
      cfield: .type
    end-structure

    typechecker typecheck

    \ Initialize the tilemap object.
    \ ( tilemap -- )
    : init 
      dup tilemap-struct 0 fill 
      init-type typecheck
    ;

  end-module \ tilemap

  begin-module tmap-params
    \ tilemap :: { }set attributes
    tilemap import

    ( size -- f )
    : (size-is-valid?) l{ 32 , 64 , 128 , 256 }l find-in 0<> ;

    \ Set map width in the tilemap object: 32, 64, 128, 256
    \ ( tilemap width -- tilemap )
    : width
      [: dup (size-is-valid?) ;] xassert
      over .width h! 
    ;

    \ Set map height in the tilemap object: 32, 64, 128, 256
    \ ( tilemap height -- tilemap )
    : height
      [: dup (size-is-valid?) ;] xassert
      over .height h! 
    ;

    \ Set the map type : TXT16/TXT256/TILE.
    \ ( tilemap type -- tilemap )
    : type
      [: dup (tmap-type-is-valid?) ;] xassert
      over .type c! ;

    \ (Re)Allocate VRAM for this tilemap to accommodate the width and height
    \ If VRAM was previously allocated for this tilemap,
    \ this VRAM will be released before reallocating VRAM.
    \ Throws x-vram-alloc-failed exception if VRAM allocation failed.
    \ ( tilemap -- )
    : }set
      [: 
        typecheck
        dup .base @ ?dup if
        vram-free
        then ( map )
        dup .base 0 swap ! ( map )
        \ Allocate VRAM (2 * width * height ) and set map base address field.
        dup .width h@ ( map width )
        over .height h@ ( map width height )
        * 2* ( map sz )
        dup vram-alloc ( map sz vram )
        dup rot 0 fill ( map vram ) 
        swap .base !
      ;] compile-or-execute
      tmap-params unimport
      [immediate]
    ;

    tilemap unimport
  end-module \ tmap-params

  \ Opening bracket for tmap{ ... }set
  ( tilemap -- tilemap )
  : tmap{ tmap-params import [immediate] ;

  \ Retrieve map width from the tilemap object.
  \ ( tilemap -- width )
  : tmap-width@ 
    tilemap :: typecheck
    tilemap :: .width h@ ;

  \ Retrieve map height from the tilemap object.
  \ ( tilemap -- height )
  : tmap-height@ 
    tilemap :: typecheck
    tilemap :: .height h@ ;

  \ Retrieve the map type from the map object
  \ ( tilemap -- type )
  : tmap-type@
    tilemap :: typecheck
    tilemap :: .type c@ ;

  \ Retrieve tilemap base address in VRAM.
  \ ( tilemap -- addr )
  : tmap-base@ 
    tilemap :: typecheck
    tilemap :: .base @ ;

  \ Deinitialize the tilemap, freeing VRAM resources.
  ( tilemap -- )
  : tmap-deinit
    tilemap :: typecheck
    dup tmap-base@ vram-free
    0 swap tilemap :: .base !
  ;

  ( tilemap -- )
  : tmap-print
    tilemap :: typecheck
    >r r@ tmap-type@ r@ tmap-height@ r@ tmap-width@ r> tmap-base@
    s" Tilemap: $%x base, %n width, %n height, %n type" printf cr
  ;

  \ Create and initialize a tilemap object.
  \ ( "name" -- )
  : <tmap> create here tilemap :: tilemap-struct allot tilemap :: init ;

  \ --- Tile Map API
  begin-module mapentry

    \ Get the address of the entry at position in given map
    : position>addr ( position tilemap -- addr )
      [ tilemap import ]
      \ Calculate 2*(row*width_ + col)
      2dup tmap-width@ swap vec2.y * ( position map y*w )
      rot vec2.x + 2* ( map offset )
      swap tmap-base@ ( offset base )
      [: dup ;] xassert
      +
      [ tilemap unimport ]
    ;

    \ Set mapentry at given position in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( mapentry position tilemap -- )
    : mapentry! position>addr h! ;

    \ Get mapentry at given in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( position tilemap -- mapentry )
    : mapentry@ position>addr h@ ;

  end-module \ mapentry

  begin-module mapentry-params
    \ mapentry :: { }set/get attributes
    mapentry import

    0 variable (position)
    0 variable (chr)
    0 variable (fg)
    0 variable (bg)
    0 variable (paloffset)
    0 variable (flip)
    0 variable (tmap)

    ( tilemap -- )
    : tmap 
      tilemap :: typecheck
      (tmap) ! ;

    ( bg -- )
    : bg (bg) ! ;

    ( fg -- )
    : fg (fg) ! ;

    ( char-code -- )
    : chr (chr) ! ;

    ( paloffset -- )
    : paloffset (paloffset) ! ;

    \ flip values: VFLIP, HFLIP, or VFLIP_HFLIP
    ( flip -- )
    : flip 
      [: dup (flip-is-valid?) ;] xassert
      (flip) ! ;

    ( vec2 -- )
    : xy (position) ! ;

    \ Write mapentry using attributes specified in {}mapentry!
    ( -- )
    : }set
      [:
        [ tilemap import ]
        (tmap) @ tmap-type@ case 
          TMAP-TILE of  
            (paloffset) @ #12 lshift ( mapentry )
            (flip) @ 3 and #10 lshift or ( mapentry )
            (chr) @ $3ff and or ( mapentry )
          endof
          TMAP-TXT16 of
            (fg) @ $f and 12 lshift
            (bg) @ $f and 8 lshift or
            (chr) @ $ff and or ( mapentry )
          endof
          (fg) @ $ff and 8 lshift
          (chr) @ $ff and or ( mapentry )
        endcase
        (position) @ ( mapentry position )
        (tmap) @ mapentry! ( )
        [ tilemap unimport ]
      ;] compile-or-execute
      mapentry-params unimport
      [immediate]
    ;

    \ Read from VRAM, mapentry specified by { <tmap> tmap <vec> position }mapentry@ and 
    \ decode it, populating fg, bg, paloffset, flip attributes.
    \ This is useful for mapentry read-modify-write operations.
    ( -- )
    : }get
      [:
        [ tilemap import ]
        (position) @ (tmap) @ mapentry@ ( mapentry )
        (tmap) @ tmap-type@ case 
          TMAP-TILE of
            dup #12 rshift (paloffset) ! ( mapentry )
            dup #10 rshift 3 and (flip) ! ( mapentry )
            $3ff and (chr) ! ( )
          endof
          TMAP-TXT16 of
            dup 12 rshift (bg) ! ( mapentry )
            dup 8 rshift $f and (fg) ! ( mapentry )
            $ff and (chr) ! ( )
          endof
          dup 8 rshift (fg) ! ( mapentry )
          $ff and (chr) ! ( )
        endcase
        [ tilemap unimport ]
      ;] compile-or-execute
      mapentry-params unimport
      [immediate]
    ;

    mapentry unimport
  end-module \ mapentry-params

  \ Opening bracket for mapentry{ ... }set and { ... }get
  ( -- )
  : mapentry{ 
    mapentry-params import [immediate] ;

  \ set a 16-bit mapentry value at row/col in given tilemap
  ( mapentry vec2 tilemap )
  : mapentry!
    tilemap :: typecheck
    mapentry :: mapentry! ;

  \ get the 16-bit mapentry value from position in given tilemap
  ( vec2 tilemap -- mapentry )
  : mapeentry@
    tilemap :: typecheck
    mapentry :: mapentry@ ;

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

  \ Unpack tile, flip and pal_offset from a 2/4/8bbp tile map entry value
  \ The color index of tile pixels is modified by the palette offset using the
  \ following logic:
  \ - Color index 0 (transparent) and 16-255 are unmodified.
  \ - Color index 1-15 is modified by adding 16 x palette offset.
  \ ( mapentry -- tile-idx flip paloffset )
  : unpack-tile
    dup $3ff and ( mapentry tile-idx )
    swap 10 rshift 3 and ( tile-idx mapentry flip )
    swap 12 rshift $f and ( tile-idx flip paloffset )
  ;

  \ -- Pixel API
  \ Getting and Setting pixels in tiles:

  begin-module pixel

    ( base width position -- ptr )
    : 8bpp-byte-ptr vec2.xy rot * + + ;

    ( base width position -- ptr )
    : 4bpp-byte-ptr vec2.xy rot * + 2/ + ;

    ( position -- bitoffset )
    : 4bpp-x-bitoffset
      vec2.x
      1 dup rot \ 1 1 x
      and - \ 1-x&1
      2 lshift ;

    ( base width position -- ptr )
    : 2bpp-byte-ptr vec2.xy rot * + 4/ + ;

    ( position -- bitoffset )
    : 2bpp-x-bitoffset
      vec2.x
      3 dup rot \ 3 3 x 
      and - \ 3-x&3 
      shl \ (3-x&3)*2
    ;

    ( base width position -- ptr )
    : 1bpp-byte-ptr vec2.xy rot * + 8/ + ;

    ( position -- bitoffset )
    : 1bpp-x-bitoffset vec2.x 7 dup rot ( 7 7 x ) and - ( 7-x&7 ) ;

  end-module \ pixel

  ( pxlval position base width -- )
  : pxl-8bpp! 
    rot pixel :: 8bpp-byte-ptr ( pxval ptr )
    c! ( )
  ;

  ( position base width -- pxlval )
  : pxl-8bpp@ 
    rot pixel :: 8bpp-byte-ptr ( ptr )
    c@ ( pxlval )
  ;

  ( pxlval position base width -- )
  : pxl-4bpp! 
    rot dup pixel :: 4bpp-x-bitoffset >r ( pxlval base y width position R: bitoffset )
    pixel :: 4bpp-byte-ptr ( pxval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    $f r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
    rot $f and ( ptr oldbytemasked pxlvalmasked R: bitoffset )
    r> lshift or ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : pxl-4bpp@
    rot dup pixel :: 4bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 4bpp-byte-ptr ( ptr R: bitoffset )
    c@ ( oldbyte R: bitoffset )
    r> rshift $f and ( pxlval )
  ;

  ( pxlval position base width -- )
  : pxl-2bpp!
    rot dup pixel :: 2bpp-x-bitoffset >r ( pxlval base width position R: bitoffset )
    pixel :: 2bpp-byte-ptr ( pxval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    3 r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
    rot 3 and ( ptr oldbytemasked pxlvalmasked R: bitoffset )
    r> lshift or ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : pxl-2bpp@
    rot dup pixel :: 2bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 2bpp-byte-ptr ( ptr R: bitoffset )
    c@ ( oldbyte R: bitoffset )
    r> rshift 3 and ( pxlval )
  ;

  ( pxlval position base width -- )
  : pxl-1bpp!
    rot dup pixel :: 1bpp-x-bitoffset >r ( pxlval base width position R: bitoffset )
    pixel :: 1bpp-byte-ptr ( pxlval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    rot r> setbit ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : pxl-1bpp@
    rot dup pixel :: 1bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 1bpp-byte-ptr ( ptr R: bitoffset )
    c@ r> rshift 1 and ( pxlval )
  ;

  \ -- Tileset API
  \ A tileset is used to represent tiles, sprite pixel data and bitmaps.

  begin-module tileset

    begin-structure tileset-struct
      field:  .base
      field:  .pxl-set
      field:  .pxl-get
      hfield: .width
      hfield: .height
      hfield: .bpp
      hfield: .#tiles
    end-structure
    
    typechecker typecheck

    \ Initialize the tileset object.
    : init ( tileset -- ) 
      dup tileset-struct 0 fill 
      init-type typecheck
    ;

    \ Retrieve the tilesize in bytes for the given tileset.
    ( tileset -- tilesize-bytes )
    : tilesize@ 
      typecheck
      dup .bpp h@ swap dup .width h@ swap .height h@ * * 8/ ;

    \ check if given position is within the width/height boundaries
    ( position tileset -- f )
    : pos-in-range?
      >r
      vec2.xy ( x y R: tileset )
      dup 0 >= swap r@ .height h@ < and ( x f R: tileset )
      swap dup 0 >= swap r> .width h@ < and ( f f )
      and
    ;
  end-module \ tileset

  begin-module tset-params
    tileset import

    \ Set the tileset width in the tileset object.
    \   - 8, 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - 320, 640 for bitmaps.
    ( tileset width -- tileset )
    : width
      \ value will be validated when applied to tileset, bitmap or sprite.
      over .width h! 
    ;

    \ Set the tileset height in the tileset object
    \   - 8 or 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - Any positive value for bitmaps.
    ( tileset height -- tileset )
    : height
      \ value will be validated when applied to tileset, bitmap or sprite.
      over .height h! 
    ;

    ( bpp -- f )
    : (bpp-is-valid?) l{ 1 , 2 , 4 , 8 }l find-in 0<> ;

    \ Set the tileset BPP in the tileset object
    \   - 1, 2, 4, 8 for regular tiles and bitmaps.
    \   - 4, 8 for sprites.
    ( tileset bpp -- tileset )
    : bpp
      [: dup (bpp-is-valid?) ;] xassert
      swap >r ( bpp R: tileset )
      dup r@ .bpp h! ( bpp R: tileset )
      case
        1 of ['] pxl-1bpp! ['] pxl-1bpp@ endof
        2 of ['] pxl-2bpp! ['] pxl-2bpp@ endof
        4 of ['] pxl-4bpp! ['] pxl-4bpp@ endof
        8 of ['] pxl-8bpp! ['] pxl-8bpp@ endof
        [: false ;] xassert
      endcase ( setter getter R: tileset )
      r@ .pxl-get ! ( R: tileset )
      r@ .pxl-set ! ( R: tileset )
      r> ( tileset )
    ;

    \ Set the number of tiles in the tileset.
    \ Range: 0..1023
    ( tileset num -- tileset )
    : tiles
      [: dup 1024 < ;] xassert ( tileset num )
      over .#tiles h! 
    ;

    \ (Re)Allocate VRAM for this tileset to accommodate
    \ #tiles, bpp, width and height.
    \ If VRAM was previously allocated for this tileset,
    \ this VRAM will be released before reallocating VRAM.
    \ Throws x-vram-alloc-failed exception if VRAM allocation failed.
    ( tileset -- )
    : }set
      [:
        typecheck
        >r ( R: tileset )
        r@ .base @ ?dup if
          vram-free
        then ( R: tileset )
        0 r@ .base ! ( R: tileset )
        r@ tilesize@ r@ .#tiles h@ * ( sz R: tileset )
        dup vram-alloc ( sz addr R: tileset )
        dup rot 0 fill ( addr R: tileset )
        r> .base ! ( )
      ;] compile-or-execute
      tset-params unimport
      [immediate]
    ;

    tileset unimport
  end-module \ tset-params

  \ Opening bracket for tset{ ... }set
  ( tileset -- tileset )
  : tset{ tset-params import [immediate] ;

  \ Given a VRAM address and a tileset, compute the tile index corresponding to that address.
  ( addr tileset -- tile-idx )
  : tset-addr>tidx
    tileset :: typecheck
    dup tileset :: .base @ 
    [: dup ;] xassert ( addr tileset baseaddr )
    rot swap - ( tileset offset )
    swap tileset :: tilesize@ ( offset tilesize )
    / ( tileidx )
  ;

  \ Given a tile index in a tileset, compute to  pointer to the pixel data of a tile in the tileset.
  \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
  \ @param tileset: Tileset object
  ( tile-idx tileset -- addr )
  : tset-tidx>addr
    tileset :: typecheck
    dup tileset :: tilesize@ ( tile-idx tileset tilesize )
    rot * ( tileset tilesize*tile-idx ) 
    swap tileset :: .base @ ( tilesize*tile-idx base )
    [: dup ;] xassert
    + ;

  \ Retrieve the tilesize in bytes for the given tileset.
  ( tileset -- tilesize-bytes )
  : tset-tilesize@ tileset :: tilesize@ ;

  \ Retrieve the tileset width from the tileset object
  ( tileset -- width )
  : tset-width@ 
    tileset :: typecheck
    tileset :: .width h@ ;

  \ Retrieve tileset base address in VRAM.
  ( tileset -- addr )
  : tset-base@ 
    tileset :: typecheck
    tileset :: .base @ ;

  \ Deinitialize the tileset, freeing VRAM resources.
  ( tileset -- )
  : tset-deinit
    tileset :: typecheck
    dup tileset :: .base @ vram-free
    0 swap tileset :: .base !
  ;

  \ Retrieve the tileset height
  ( tileset -- height )
  : tset-height@ 
    tileset :: typecheck
    tileset :: .height h@ ;

  \ Retrieve the tileset BPP from the tileset object.
  ( tileset -- bpp )
  : tset-bpp@ 
    tileset :: typecheck
    tileset :: .bpp h@ ;

  ( tileset -- #tiles )
  : tset-#tiles@ 
    tileset :: typecheck
    tileset :: .#tiles h@ ;

  ( tileset -- )
  : tset-print
    tileset :: typecheck
    >r r@ tset-#tiles@ r@ tset-bpp@ r@ tset-height@ r@ tset-width@ r> tset-base@
    s" Tileset: $%x base, %n width, %n height, %n bpp, %n tiles" printf cr
  ;

  \ Create and initialize a tileset object.
  \ ( "name" -- )
  : <tset> create here tileset :: tileset-struct allot tileset :: init ;

  begin-module pxl-params

    0 variable (position)
    0 variable (tidx)
    0 variable (bitmapaddr)
    0 variable (color)
    0 variable (tset)

    ( tileset -- )
    : tset 
      tileset :: typecheck
      (tset) ! ;

    \ tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
    ( tile-idx -- )
    : tidx (tidx) ! ;

    ( color -- ) 
    : color (color) ! ;

    ( vec2 -- ) 
    : xy (position) ! ;

    \ Set a pixel in the given tile.
    ( -- )
    : }set
      [:
        (color) @
        (position) @ [: dup (tset) @ tileset :: pos-in-range? ;] xassert
        (tidx) @ [: dup (tset) @ tset-#tiles@ <= ;] xassert
        (tset) @ tset-tidx>addr
        (tset) @ tset-width@
        (tset) @ tileset :: .pxl-set @
        ( color position addr width pxl-setter )
        execute
      ;] compile-or-execute
      pxl-params unimport
      [immediate]
    ;

    \ Read the pixel color from the given position on the given tile.
    \ ( -- color )
    : }get
      [:
        (position) @ [: dup (tset) @ tileset :: pos-in-range? ;] xassert
        (tidx) @ [: dup (tset) @ tset-#tiles@ <= ;] xassert
        (tset) @ tset-tidx>addr 
        (tset) @ tset-width@
        (tset) @ tileset :: .pxl-get @
        ( position addr width pxl-getter )
        execute ( color )
        dup (color) ! ( color )
      ;] compile-or-execute
      pxl-params unimport
      [immediate]
    ;
  end-module \ pxl-params

  \ Opening bracket for pxl{ ... }set/get
  ( -- tileset )
  : pxl{ pxl-params import [immediate] ;

  \ -- Sprite API

  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS constant SPR-DIS \ Sprite disabled. 
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 constant SPR-BG-L0 \ Between background and L0. 
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 constant SPR-L0-L1 \ Between L0 and L1. 
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 constant SPR-L1 \ In front of L1. 

  : (zdepth-is-valid?) l{ SPR-DIS , SPR-BG-L0 , SPR-L0-L1 , SPR-L1 }l find-in 0<> ;

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

    typechecker typecheck

    \ Calculate the sprite attribute RAM address from the given sprite id.
    \ ( id -- addr )
    : id>ram 8 * VERA_SPRITE_RAM_BASE + ;

    \ Calculate the sprite id from the given sprite attribute RAM address.
    \ ( addr -- id )
    : ram>id VERA_SPRITE_RAM_BASE - 8 / ;

    : init ( sprite-idx sprite -- )
      [: over #SPRITES u< ;] xassert
      dup sprite-struct 0 fill ( sprite-idx sprite )
      swap id>ram ( sprite ramaddr )
      over .attr-ram-ptr ! ( sprite )
      init-type typecheck
    ;

    \ ( tilesize - tilesize-encoded )
    : sizeenc log2 3 - ;

    \ ( tilesize-encoded -- tilesize )
    : sizedec 3 + 1<< ;

    ( size -- f )
    : spritesize-is-valid? l{ 8 , 16 , 32 , 64 }l find-in 0<> ;

    \ Set the sprite width
    \ ( width sprite -- )
    : width! 
      [: over spritesize-is-valid? ;] xassert
      swap sizeenc 
      swap .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH! 
    ;

    \ Set the sprite height
    \ ( height sprite -- )
    : height!
      [: over spritesize-is-valid? ;] xassert
      swap sizeenc swap .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT! 
    ;

    ( bpp -- f )
    : bpp-is-valid? l{ 4 , 8 }l find-in 0<> ;

    \ Set the sprite's BPP. 8 or 4.
    \ ( bpp sprite -- )
    : bpp! 
      [: over bpp-is-valid? ;] xassert
      swap 8 = swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE! 
    ;

    \ Set the sprite's VRAM address
    \ ( addr sprite -- )
    : addr!
      swap VERA_VRAM_BASE - 5 rshift ( sprite addr )
      swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR!
    ;
    end-module

    begin-module spr-params
      sprite import

      \ ( sprite vec2 -- sprite )
      : xy 
          swap >r ( vec2 R: sprite )
          vec2.xy ( x y R: sprite )
          r@ .attr-y h!
          r@ .attr-x h!
          r> ( sprite )
      ;


      \ flip values: VFLIP, HFLIP, or VFLIP_HFLIP
      \ ( sprite flip -- sprite )
      : flip 
        [: dup (flip-is-valid?) ;] xassert
        over .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP! ;

      \ ( sprite zdepth -- sprite )
      : z
        [: dup (zdepth-is-valid?) ;] xassert
        over .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH! ;

      \ ( sprite colmask -- sprite )
      : colmask over .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK! ;

      \ ( sprite paloffset -- sprite )
      : paloffset over .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET! ;

      \ Set the tile index to be used in the sprite object.
      \ ( sprite tile-idx -- sprite )
      : tidx
        2dup swap .tile-idx ! ( sprite tile-idx )
        \ Compute and set the address attribute if we have a tileset.
        \ If we don't have a tileset yet, this is deferred until the tileset
        \ is specified.
        over .tileset @ ?dup if ( sprite tile-idx tileset )
          tset-tidx>addr ( sprite addr ) 
          over addr! ( sprite )
        else
          drop ( sprite )
        then
      ;
  
      \ Set the tileset to be used in the sprite object.
      \ When modifying the tileset used by a sprite object, keep in mind that
      \ the corresponding tile index (tidx, see above) has to be valid (within
      \ range) for the new tileset.
      : tset ( sprite tileset -- sprite )
        tileset :: typecheck
        swap >r ( tileset R : sprite )
        r@ .tile-idx @ ( tileset tile-idx R: sprite )
        over tset-tidx>addr r@ addr! ( tileset R: sprite )
        dup tset-bpp@ r@ bpp! ( tileset R: sprite )
        dup tset-width@ r@ width! ( tileset R: sprite )
        dup tset-height@ r@ height! ( tileset R: sprite )
        r@ .tileset ! ( R: sprite )
        r>
      ;

      \ Commit the sprite's attributes to hardware, 
      \ i.e. to the sprite attribute RAM.
      \ ( sprite -- )
      : }set
        [:
          typecheck
          dup .attr-ram-ptr @ ( sprite attr-ram-addr )
          [: dup xassert ;] ( sprite attr-ram-addr )
          swap .attr-addr ( attr-ram-addr attr-addr )
          2dup @ ( attr-ram-addr attr-addr attr-ram-addr attr0/1 ) 
          swap ! ( attr-ram-addr attr-addr )
          cell+ @ ( attr-ram-addr attr1/2 )
          swap cell+ ( attr1/2 attr-ram-addr' )
          !
        ;] compile-or-execute
        spr-params unimport
        [immediate]
      ;

      sprite unimport
    end-module \ spr-params

  \ Opening bracket for spr{ ... }set
  ( sprite -- sprite )
  : spr{ spr-params import [immediate] ;

  \ Get the sprite's VRAM address
  \ ( sprite -- addr )
  : spr-addr@ 
    sprite :: typecheck
    sprite :: .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR@ 
    5 lshift VERA_VRAM_BASE +
  ;

  \ Retrieve the sprite id from the sprite object.
  : spr-id@ ( sprite -- id ) 
    sprite :: typecheck
    sprite :: .attr-ram-ptr @ sprite :: ram>id ;

  \ Get the sprite's current coordinates.
  \ ( sprite -- vec2 )
  : spr-xy@ 
    sprite :: typecheck
    dup sprite :: .attr-x h@ swap sprite :: .attr-y h@ vec2 ;

  \ Get the sprite width
  \ ( sprite -- width )
  : spr-width@ 
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH@ sprite :: sizedec ;

  \ Get the sprite height
  \ ( sprite -- height )
  : spr-height@
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT@ sprite :: sizedec ;

  \ ( sprite -- flip )
  : spr-flip@ 
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP@ ;

  \ ( sprite -- zdepth )
  : spr-z@ 
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH@ ;

  \ ( sprite -- colmask )
  : spr-colmask@ 
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK@ ;

  \ ( sprite -- paloffset )
  : spr-paloffset@ 
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET@ ;

  \ Get the sprite's BPP (8 or 4).
  \ ( sprite -- bpp )
  : spr-bpp@ 
    sprite :: typecheck
    sprite :: .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE@ if 8 else 4 then ;

  \ Retrieve the tileset corresponding to this sprite.
  \ ( sprite -- tileset )
  : spr-tset@ 
    sprite :: typecheck
    sprite :: .tileset @ ;

  \ Retrieve the tile-idx corresponding to this sprite.
  \ ( sprite -- tile-idx )
  : spr-tidx@ sprite :: .tile-idx @ ;

  ( sprite -- )
  : spr-print
    sprite :: typecheck
    >r 
    r@ spr-colmask@ r@ spr-z@ r@ spr-flip@ r@ spr-height@ r@ spr-width@ r@ spr-xy@ vec2.xy swap r@ spr-id@
    s" sprite: %n id, %n x, %n y, %n w, %n h, %n flip, %n z, $%x colmask" printf cr
    r@ spr-tidx@ r@ spr-tset@ r@ spr-addr@ r@ spr-bpp@ r> spr-paloffset@
    s" %n paloffset, %n bpp, $%x addr, $%x tset, %n tidx" printf cr
  ;

  \ Create and initialize a sprite object.
  \ sprite-idx must be in range 0..NUM_SPRITES-1.
  \ ( sprite-idx "name" -- )
  : <spr> 
    create here sprite :: sprite-struct allot ( sprite-idx sprite )
    sprite :: init ;

  begin-module layer

    begin-structure layer-struct
      field:  .tileset
      field:  .tilemap
      hfield: .tile-idx
      cfield: .id
    end-structure

    typechecker typecheck

    \ Initialize a layer object
    : init ( id layer -- )
      [: over #LAYERS < ;] xassert ( id layer )
      dup layer-struct 0 fill ( id layer )
      tuck .id c!
      init-type typecheck
    ;

    \ Set tilemap base address for the given layer
    \ ( addr layer-id -- )
    : tilemap-base!
      swap VERA_VRAM_BASE - 9 rshift ( layer-id vram-base )
      swap if VERA_L1_MAPBASE! else VERA_L0_MAPBASE! then
    ;

    : sizeenc log2 5 - [inline] ;

    : sizedec 5 + 1<< [inline] ;

    \ Set tilemap width for given layer
    \ ( width layer-id -- )
    : tilemap-width!
      swap sizeenc
      swap if VERA_L1_CONFIG_MAP_WIDTH! else VERA_L0_CONFIG_MAP_WIDTH! then
    ;

      ( height layer-id -- )
    : tilemap-height!
      swap sizeenc
      swap if VERA_L1_CONFIG_MAP_HEIGHT! else VERA_L0_CONFIG_MAP_HEIGHT! then
    ;

      ( f layer-id -- )
    : t256c! if VERA_L1_CONFIG_T256C! else VERA_L0_CONFIG_T256C! then ;

    ( bpp - bpp-encoded )
    : bppenc log2 ;

    ( bpp-encoded -- bpp )
    : bppdec 1<< ;

    ( bpp layer-id -- )
    : bpp!
      swap bppenc ( layer bpp-encoded )
      swap if VERA_L1_CONFIG_COLORDEPTH! else VERA_L0_CONFIG_COLORDEPTH! then
    ;

    ( f layer-id -- )
    : bitmap-mode! if VERA_L1_CONFIG_BITMAPMODE! else VERA_L0_CONFIG_BITMAPMODE! then ;

    ( paloffset layer-id -- )
    : paloffset! if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET! else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then ;

    \ In bitmap mode, true sets bitmap width 640, false 320.
    \ In tile mode, true sets tile width 16, false 8.
    \ ( f layer-id -- )
    : tile-width! 
      if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH! else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH! then ;

    \ True sets tile height 16, false 8.
    \ ( f layer-id -- )
    : tile-height! if VERA_L1_TILEBASE_TILE_HEIGHT! else VERA_L0_TILEBASE_TILE_HEIGHT! then ;

    ( addr layer-id -- )
    : tile-base!
      swap VERA_VRAM_BASE - 11 rshift ( layer-id addr )
      swap if VERA_L1_TILEBASE_TILE_BASEADDR! else VERA_L0_TILEBASE_TILE_BASEADDR! then
    ;

    ( size -- f )
    : tilesize-is-valid? l{ 8 , 16 }l find-in 0<> ;

    \ Configure given tileset into given layer.
    ( layer-id tileset -- )
    : tileset!
      >r ( layer-id R: tileset )
      r@ tset-bpp@ over bpp! ( layer-id R: tileset )
      false over bitmap-mode! ( layer-id R: tileset )
      r@ tset-width@ ( layer-id width R: tileset )
      [: dup tilesize-is-valid? ;] xassert
      16 = swap over tile-width! ( layer-id R: tileset )
      r@ tset-height@ ( layer-id height R: tileset )
      [: dup tilesize-is-valid? ;] xassert
      16 = over tile-height! ( layer-id R: tileset )
      [: r> tset-base@ dup ;] xassert ( layer-id base R: tileset )
      swap tile-base!
    ;

    ( size -- f )
    : bitmap-width-is-valid? l{ 320 , 640 }l find-in 0<> ;

    \ Configure given bitmap (identified by a bitmap descriptor) into the given layer.
    ( tileset tile-idx layer-id -- )
    : bitmap!
      >r ( tileset tile-idx R: layer-id )
      over tset-tidx>addr r@ tile-base! ( tileset R: layer-id )
      dup tset-bpp@ r@ bpp! ( tileset R: layer-id )
      tset-width@ ( width R: layer-id )
      [: dup bitmap-width-is-valid? ;] xassert ( width R: layer-id )
      640 = r@ tile-width! ( f R: layer-id )
      0 r@ tile-height! ( R: layer-id )
      true r> bitmap-mode! ( )
    ;
  end-module \ layer

  begin-module layer-params
    layer import

    \ Set the tilemap to be used by this layer (configuring tilemapmode)
    ( layer tmap -- layer )
    : tmap
      tilemap :: typecheck
      over .tilemap ! ;

    \ Set the tileset to be used by this layer (tilemapmode and bitmapmode)
    ( layer tileset -- layer )
    : tset
      tileset :: typecheck
      over .tileset ! ;

    \ Set the tile index to be used by this layer (bitmapmode)
    ( layer tile-idx -- layer )
    : tidx over .tile-idx h! ;

    \ Configure the layer in tilemap mode. tmap and tset must be specified.
    ( layer -- )
    : }tilemap-mode
      [:
        typecheck
        dup .id c@ ( layer layer-id )
        over .tilemap @ ( layer layer-id tmap )
        [: dup ;] xassert
        2dup tmap-type@ TMAP-TXT256 = ( layer layer-id tmap layer-id t256c )
        swap t256c! ( layer layer-id map )
        2dup tmap-width@ swap tilemap-width! ( layer layer-id tmap )
        2dup tmap-height@ swap tilemap-height! ( layer layer-id tmap )
        tmap-base@ [: dup ;] xassert ( layer layer-id base )
        over tilemap-base! ( layer layer-id )
        swap .tileset @ ( layer-id tileset )
        [: dup ;] xassert
        tileset!
      ;] compile-or-execute
      layer-params unimport
      [immediate]
    ;

    \ Configure the layer in bitmap mode. tset and tidx must be specified.
    ( layer -- )
    : }bitmap-mode
      [: 
        typecheck
        dup .id c@ ( layer layer-id )
        over .tileset @ ( layer layer-id tileset )
        [: dup ;] xassert
        rot .tile-idx h@ ( layer-id tileset tile-idx )
        rot bitmap!
      ;] compile-or-execute
      layer-params unimport
      [immediate]
    ;

    layer unimport
  end-module \ layer-params 

  \ opening brack for layer{ ... }tilemap-mode or layer :: { ... }bitmap-mode
  ( layer -- layer )
  : layer{ layer-params import [immediate] ;

  \ l0 and l1 are the objects to be passed into the public words below.
  create l0 layer :: layer-struct allot
  create l1 layer :: layer-struct allot
  0 l0 layer :: init
  1 l1 layer :: init

  : layer-enable ( f layer -- ) 
    layer :: typecheck
    layer :: .id c@ if VERA_DC_VIDEO_L1_ENABLE! else VERA_DC_VIDEO_L0_ENABLE! then ;

  : layer-enabled? ( layer -- f ) 
    layer :: typecheck
    layer :: .id c@ if VERA_DC_VIDEO_L1_ENABLE@ else VERA_DC_VIDEO_L0_ENABLE@ then 0<> ;

    ( layer -- addr )
  : layer-tmap-base@
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_MAPBASE@ else VERA_L0_MAPBASE@ then
    9 lshift VERA_VRAM_BASE +
  ;

  \ ( layer -- width )
  : layer-tmap-width@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_MAP_WIDTH@ else VERA_L0_CONFIG_MAP_WIDTH@ then layer :: sizedec ;

    ( layer -- height )
  : layer-tmap-height@
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_CONFIG_MAP_HEIGHT@ else VERA_L0_CONFIG_MAP_HEIGHT@ then layer :: sizedec ;

    ( layer -- f )
  : layer-t256c@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_T256C@ else VERA_L0_CONFIG_T256C@ then 0<> ;

  ( layer -- bpp )
  : layer-bpp@
    layer :: typecheck
    layer :: .id c@
    swap if VERA_L1_CONFIG_COLORDEPTH@ else VERA_L0_CONFIG_COLORDEPTH@ then
    layer :: bppdec
  ;

  ( layer -- f )
  : layer-bitmap-mode@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_BITMAPMODE@ else VERA_L0_CONFIG_BITMAPMODE@ then 0<> ;

  ( layer -- paloffset )
  : layer-paloffset@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET@ else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET@ then ;

  ( paloffset layer -- )
  : layer-paloffset! 
    layer :: typecheck
    layer :: .id c@ layer :: paloffset!
  ;

  ( hscroll layer -- )
  : layer-hscroll!
    layer :: typecheck
    dup layer :: .id c@ ( hscroll layer id )
    swap layer-bitmap-mode@ if ( hscroll id )
      if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( hscroll addr )
      !
    else ( hscroll layer id )
      if VERA_L1_HSCROLL_HSCROLL_7_0! else VERA_L0_HSCROLL_HSCROLL_7_0! then
    then
  ;

  ( layer -- hscroll )
  : layer-hscroll@
    layer :: typecheck
    dup layer :: .id c@ ( layer id )
    swap layer-bitmap-mode@ if ( id )
      if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( addr )
      @
    else ( id )
      if VERA_L1_HSCROLL_HSCROLL_7_0@ else VERA_L0_HSCROLL_HSCROLL_7_0@ then
    then
  ;

  ( hscroll layer -- )
  : layer-vscroll! 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_VSCROLL! else VERA_L0_VSCROLL! then ! ;

  ( layer -- vscroll )
  : layer-vscroll@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_VSCROLL@ else VERA_L0_VSCROLL@ then ;

  ( layer -- width )
  : layer-tile-width@ 
    layer :: typecheck
    dup layer :: .id c@ 
    if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@ else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@ then ( layer w )
    1+ ( layer w=1|2 )
    swap layer-bitmap-mode@ if 320 else 8 then *
  ;

  ( layer -- height )
  : layer-tile-height@ 
    layer :: typecheck
    layer :: .id c@ if VERA_L1_TILEBASE_TILE_HEIGHT@ else VERA_L0_TILEBASE_TILE_HEIGHT@ then
    1+ 8 *
  ;

  ( layer -- addr-id )
  : layer-tile-base@
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_TILEBASE_TILE_BASEADDR@ else VERA_L0_TILEBASE_TILE_BASEADDR@ then
    11 lshift VERA_VRAM_BASE +
  ;

  \ Retrieve tileset used by this layer
  ( layer -- tileset )
  : layer-tset@ 
    layer :: typecheck
    layer :: .tileset @ ;

  \ Retrieve tile-idx used by this layer (bitmap mode).
  ( layer -- tileset )
  : layer-tidx@ 
    layer :: typecheck
    layer :: .tile-idx h@ ;

  \ Retrieve tilemap used by this layer (tilemap mode).
  ( layer -- tilemap )
  : layer-tmap@ 
    layer :: typecheck
    layer :: .tileset @ ;

  ( layer -- )
  : layer-print
    layer :: typecheck
    >r 
    r@ layer-enabled? if s" enabled" else s" disabled" then printf cr
    r@ layer-bitmap-mode@ if
      ." bitmap mode" cr
      r@ layer-tidx@ r@ layer-tset@ r@ layer-tile-base@ r@ layer-tile-height@ r@ layer-tile-width@ 
      r@ layer-vscroll@ r@ layer-hscroll@ r@ layer-paloffset@ r> layer-bpp@
      s" %n bpp, %n paloffset, %n hscroll, %n vscroll, %n width, %n height, $%x base, $%x tset, %n tidx" 
      printf cr
    else
      ." tile mode" cr
      r@ layer-tmap@ r@ layer-tidx@ r@ layer-tset@ r@ layer-tile-base@ r@ layer-tile-height@ r@ layer-tile-width@ 
      r@ layer-vscroll@ r@ layer-hscroll@ r@ layer-bpp@
      s" %n bpp, %n hscroll, %n vscroll, %n width, %n height, $%x base, $%x tset, %n tidx, $%x tilemap " printf cr
      r@ layer-tmap@ r@ layer-t256c@ r@ layer-tmap-height@ r@ layer-tmap-width@ r@ layer-tmap-base@
      s" $%x tmap-base, %n tmap-width, %n tmap-height, %n t256c" printf cr
    then
  ;

  ( f -- )
  : line-capture-enable VERA_CTRL_STATUS_CAPTURE_EN! ;

  ( -- f )
  : line-capture-enabled? VERA_CTRL_STATUS_CAPTURE_EN@ 0<> ;

  \  Read the RGB value of a pixel on the captured line.
  \ @param x: the pixel's x position. Range: 0..639.
  \ @return: 12-bit RGB triple.
  : line-capture-pxl@ ( x -- rgb ) 4 * VERA_CAPTURE_RAM_BASE + @ $fff and ;

  \ --- Interrupt Subsystem ---
  VERA_IEN_VAL_VSYNC constant IRQ-VSYNC-MASK
  VERA_IEN_VAL_LINE constant IRQ-LINE-MASK
  VERA_IEN_VAL_SPRCOL constant IRQ-SPRCOL-MASK

  \ Enable IRQs. The passed in mask will be OR'd with the installed mask.
  \ @param mask: bitwise OR of VERA_IRQs to enable.
  : irq-enable ( mask -- ) VERA_IEN_ADDR tuck @ or ! ;

  \ Disable IRQs. The passed in mask will be inverted and  AND'd with the
  \ installed mask.
  \ @param mask: bitwise OR of VERA_IRQs to disable.
  : irq-disable ( mask -- ) VERA_IEN_ADDR tuck @ swap bic ! ;

  \ Retrieve the enabled IRQs bitmask.
  \ @return: a bitmask of enabled VERA_IRQs.
  : irq-enabled ( -- mask ) VERA_IEN_ADDR @ ;

  \ Retrieve the active IRQs.
  \ @return: a bitmask of active VERA_IRQs.
  : irq-get ( -- active-mask ) VERA_ISR_ADDR @ VERA_IEN_ADDR @ and ;

  \ Acknowledge IRQs.
  \ @param mask: bitwise OR of VERA_IRQs to acknowledge.
  : irq-ack ( mask -- ) VERA_ISR_ISR! ;

  \ Set/Get the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
  \ enabled.
  \ @param scanline: scanline number on which the trigger the line IRQ, must be
  \ <= VERA_SCANLINE_MAX.
  : irqline-set ( scanline -- ) VERA_IRQLINE! ;
  : irqline-get ( -- scanline ) VERA_IRQLINE@ ;
  : scanline-get ( -- scanline ) VERA_SCANLINE@ ;

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

  \ Shadow memory. VERA's palette memory is write-only.
  create (shadow-palette)
  include /forth/vera-palette.fs

  ( rgb idx -- )
  : (pal!)
    swap ( idx rgb )
    $fff and ( idx rgbmasked )
    swap ( rgbmasked idx )
    4 * VERA_PALETTE_RAM_BASE + !
  ;

  \ Expects standard 4-bit color fields mapped linearly
  \ Write an entry into the palette.
  \ @param idx: the palete color index
  \ @param rgb: the 12-bit RGB triple
  : pal! ( rgb idx -- )
    2dup 2* (shadow-palette) + h!
    (pal!)
  ;

  \ Read the RGB value of a palette entry
  \ @param idx: the palete color index:
  \ @return: the 12-bit RGB triple
  : pal@ ( idx -- rgb ) 2* (shadow-palette) + h@ ;

  \ Load the shadow-palette into VERA's palette memory.
  : pal-init
    256 0 do
      i pal@ i (pal!)
    loop
  ;

  pal-init

  \ Load a palette into VERA palette memory
  \ addr points to a block of 256 half-words, each half-word specifying a 12-bit rgb value
  \ corresponding to its index.
  ( addr -- )
  : pal-load
    move (shadow-palette) 512 \ Copy it to the shadow-palette first
    pal-init \ Then install shadow-palette into VERA palette memory.
  ;

  \ -- VERA top-level definitions
  : display-enable ( flag -- ) if 1 else 0 then VERA_DC_VIDEO_OUTPUT_MODE! ;

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

