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
  VERA_SPRITE_ATTR_FLAGS_FLIP_VFLIP constant VFLIP
  VERA_SPRITE_ATTR_FLAGS_FLIP_HFLIP  constant HFLIP
  VERA_SPRITE_ATTR_FLAGS_FLIP_HFLIP_VFLIP  constant VFLIP_HFLIP

  \ --- VRAM ---

  begin-module vram

    : x-alloc-failed ." VRAM allocation failed." cr ;

    #2048 constant BLOCK_SZ_BYTES
    BLOCK_SZ_BYTES log2 constant LOG_BLOCK_SZ
    VERA_VRAM_SIZE_BYTES LOG_BLOCK_SZ rshift constant #BLOCKS

    create blocks_ #BLOCKS chars allot

    : reset blocks_ #BLOCKS 0 fill ;

    reset


    \ --- VRAM Allocation Subsystem ---
    \ In the blocks_ array, at offset, attempt to find requested blocks.
    \ Return actual # of blocks found (might be less than requested).
    : (find-free-blocks) ( requested offset -- found )
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
    : (find-free-chunk)
      #BLOCKS 0 do \ Start scanning from offset 0 ( #blocks )
        \ Attempt to find #blocks starting from offset i.
        dup i (find-free-blocks) ( #blocks remaining-blocks )
        0= if \ If no remaining-blocks, we're done. ( #blocks )
          drop i
          unloop exit
        then
      loop
      drop -1
    ;

    ( #blocks block-idx -- )
    : (allocate-blocks)
      blocks_ + ( #blocks vram_block_ptr )
      2dup c! ( #blocks vram_block_ptr )
      1+ swap 1- $ff fill ( )
    ;

    : (find-alloc-blocks) ( #blocks -- block-idx )
      dup (find-free-chunk) ( #blocks block-idx )
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
      [ BLOCK_SZ_BYTES 1- ] literal + LOG_BLOCK_SZ rshift ( #blocks )
      (find-alloc-blocks) ( block-idx )
      \ Convert to address
      LOG_BLOCK_SZ lshift VERA_VRAM_BASE +
    ;

    \ ( block-idx -- )
    : (free)
      blocks_ + ( vram_block_ptr )
      dup c@ ( vram_block_ptr #blocks )
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
      hfield: .width
      hfield: .height
      cfield: .type
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

    \ Deinitialize the tilemap, freeing VRAM resources.
    ( tilemap -- )
    : deinit
      dup base@ vram :: free
      0 swap .base !
    ;
  
    ( tilemap -- )
    : print
      >r r@ type@ r@ height@ r@ width@ r> base@
      s" Tilemap: $%x base, %n width, %n height, %n type" printf cr
    ;

    begin-module params
      \ tilemap :: { }set attributes

      ( size -- f )
      : (size-is-valid?) l{ 32 , 64 , 128 , 256 }l find-in 0<> ;

      \ Set map width in the tilemap object: 32, 64, 128, 256
      \ ( tilemap width -- tilemap )
      : width
        dup (size-is-valid?) ?assert
        over .width h! 
      ;

      \ Set map height in the tilemap object: 32, 64, 128, 256
      \ ( tilemap height -- tilemap )
      : height
        dup (size-is-valid?) ?assert
        over .height h! 
      ;

      \ Map types
      TXT16 constant TXT16
      TXT256 constant TXT256
      TILE constant TILE

      ( type -- f )
      : (type-is-valid?) l{ TXT16 , TXT256 , TILE }l find-in 0<> ;

      \ Set the map type : TXT16/TXT256/TILE.
      \ ( tilemap type -- tilemap )
      : type
        dup (type-is-valid?) ?assert
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
        params unimport
        [immediate]
      ;
    end-module \ tilemap :: params
  end-module \ tilemap

  \ Create and initialize a tilemap object.
  \ ( "name" -- )
  : <tilemap> create here tilemap :: tilemap-struct allot tilemap :: init ;

  \ Opening bracket for tilemap{ ... }set
  ( tilemap -- tilemap )
  : tilemap{ tilemap :: params import [immediate] ;

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

    begin-module params
      \ mapentry :: { }set/get attributes

      0 variable (position)
      0 variable (chr)
      0 variable (color)
      0 variable (paloffset)
      0 variable (flip)

      ( tilemap bg -- tilemap )
      : bg
        [ tilemap import ]
        over type@ TXT16 = ?assert
        4 lshift ( tilemap bgshifted )
        (color) @ ( tilemap bshifted oldcolor )
        $f and or     ( tilemap newcolor )
        (color) ! ( tilemap )
        [ tilemap unimport ]
      ;

      ( tilemap fg -- tilemap )
      : fg
        [ tilemap import ]
        over type@ TXT16 = if
          $f and         ( tilemap fgmasked )
          (color) @ ( tilemap fgmasked oldcolor )
          $f0 and or      ( tilemap newcolor )
        then
        (color) ! ( tilemap )
        [ tilemap unimport ]
      ;

      ( tilemap tile-idx -- tilemap )
      : chr (chr) ! ;

      ( tilemap paloffset -- tilemap )
      : paloffset
        [ tilemap import ]
        over type@ TILE = ?assert
        (paloffset) !
        [ tilemap unimport ]
      ;

      \ flip values: VFLIP, HFLIP, or VFLIP_HFLIP
      ( tilemap flip -- tilemap )
      : flip
        [ tilemap import ]
        over type@ TILE = ?assert
        (flip) !
        [ tilemap unimport ]
      ;

      ( tilemap vec2 -- tilemap )
      : xy over (position) ! ;

      \ Write mapentry using attributes specified in {}mapentry! block.y
      ( tilemap -- )
      : }set
        [:
          [ tilemap import ]
          dup type@ TILE = if ( tilemap )
            dup (paloffset) @ #12 lshift ( tilemap mapentry )
            over (flip) @ 3 and #10 lshift or ( tilemap mapentry )
            over (chr) @ $3ff and or ( tilemap mapentry )
          else
            dup (color) @ 8 lshift ( tilemap mapentry )
            over (chr) @ ( tilemap mapentry tileidx )
            $ff and or ( tilemap mapentry )
          then
          over (position) @ ( tilemap mapentry position )
          rot (mapentry!) ( )
          [ tilemap unimport ]
        ;] compile-or-execute
        params unimport
        [immediate]
      ;

      \ Read from VRAM, mapentry specified by { <row> <col> position }mapentry@ and 
      \ decode it, populating fg, bg, paloffset, flip attributes.
      \ This is useful for mapentry read-modify-write operations.
      ( tilemap -- )
      : }get
        [:
          [ tilemap import ]
          (position) @ over (mapentry@) ( tilemap mapentry )
          swap type@ TILE = if ( mapentry )
            dup #12 rshift (paloffset) ! ( mapentry )
            dup #10 rshift 3 and (flip) ! ( mapentry )
            $3ff and (chr) ! ( )
          else
            dup 8 rshift (color) ! ( mapentry )
            $ff and (chr) ! ( )
          then
          [ tilemap unimport ]
        ;] compile-or-execute
        params unimport
        [immediate]
      ;
    end-module \ mapentry :: params

    \ set a 16-bit mapentry value at row/col in given tilemap
    ( mapentry vec2 tilemap )
    : set (mapentry!) ;

    \ get the 16-bit mapentry value from position in given tilemap
    ( vec2 tilemap -- mapentry )
    : get (mapentry@) ;

  end-module \ mapentry

  \ Opening bracket for mapentry{ ... }set and { ... }get
  ( tilemap -- tilemap )
  : mapentry{ mapentry :: params import [immediate] ;

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
      hfield: .width
      hfield: .height
      hfield: .bpp
      hfield: .#tiles
    end-structure

    \ Initialize the tileset object. This must be done only once.
    : init ( tileset -- ) tileset-struct 0 fill ;

    \ Retrieve the tileset width from the tileset object
    ( tileset -- width )
    : width@ .width h@ ;

    \ Retrieve tileset base address in VRAM.
    ( tileset -- addr )
    : base@ .base @ ;

    \ Deinitialize the tileset, freeing VRAM resources.
    ( tileset -- )
    : deinit
      dup base@ vram :: free
      0 swap .base !
    ;

    \ Retrieve the tileset height
    ( tileset -- height )
    : height@ .height h@ ;

    \ Retrieve the tileset BPP from the tileset object.
    ( tileset -- bpp )
    : bpp@ .bpp h@ ;

    ( tileset -- #tiles )
    : #tiles@ .#tiles h@ ;

    ( tileset -- )
    : print
      >r r@ #tiles@ r@ bpp@ r@ height@ r@ width@ r> base@
      s" Tileset: $%x base, %n width, %n height, %n bpp, %n tiles" printf cr
    ;

    \ Retrieve the tilesize in bytes for the given tileset.
    ( tileset -- tilesize-bytes )
    : tilesize@ dup bpp@ swap dup width@ swap height@ * * 8/ ;

    \ Given a VRAM address and a tileset, compute the tile index corresponding to that address.
    ( addr tileset -- tile-idx )
    : addr>tidx
      dup base@ dup ?assert ( addr tileset baseaddr )
      rot swap - ( tileset offset )
      swap tilesize@ ( offset tilesize )
      / ( tileidx )
    ;

    \ Given a tile index in a tileset, compute to  pointer to the pixel data of a tile in the tileset.
    \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
    \ @param tileset: Tileset object
    ( tile-idx tileset -- addr )
    : tidx>addr
      dup tilesize@ ( tile-idx tileset tilesize )
      rot * ( tileset tilesize*tile-idx ) 
      swap base@ ( tilesize*tile-idx base )
      dup ?assert
      + ;

    begin-module params

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
        dup (bpp-is-valid?) ?assert
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
          >r ( R: tileset )
          r@ base@ ?dup if
            vram :: free
          then ( R: tileset )
          0 r@ .base ! ( R: tileset )
          r@ tilesize@ r@ #tiles@ * 
          vram :: alloc ( addr R: tileset )
          r> .base ! ( R: tileset )
        ;] compile-or-execute
        params unimport
        [immediate]
      ;
    end-module \ tileset :: params

  end-module \ tileset

  \ Create and initialize a tileset object.
  \ ( "name" -- )
  : <tileset> create here tileset :: tileset-struct allot tileset :: init ;

  \ Opening bracket for tileset{ ... }set
  ( tileset -- tileset )
  : tileset{ tileset :: params import [immediate] ;

  pixel continue-module
    begin-module params

      0 variable (position)
      0 variable (bitmapaddr)
      0 variable (color)

      \ @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
      \ @param tileset: Tileset object
      ( tileset tile-idx -- tileset )
      : tidx
        2dup swap tileset :: #tiles@ <= ?assert ( tileset tile-idx )
        over tileset :: tidx>addr ( tileset addr )
        (bitmapaddr) ! ( tileset )
      ;

      ( tileset color -- tileset )
      : color (color) ! ;

      ( tileset vec2 -- tileset )
      : xy (position) ! ;

      \ Set a pixel in the given tile.
      ( tileset -- )
      : }set
        [:
          >r ( R: tileset )
          (color) @
          (position) @
          (bitmapaddr) @
          r@ tileset :: .width h@ 
          r> tileset :: .pxl-set @
          ( color position addr width pxl-setter )
          execute
        ;] compile-or-execute
        params unimport
        [immediate]
      ;

      \ Read the pixel color from the given position on the given tile.
      \ ( tileset -- color )
      : }get
        [:
          >r ( R: tileset )
          (position) @
          (bitmapaddr) @
          r@ tileset :: .width h@
          r> tileset :: .pxl-get @
          ( position addr width pxl-getter )
          execute ( color )
          dup (color) ! ( color )
        ;] compile-or-execute
        params unimport
        [immediate]
      ;
    end-module \ pixel :: params

  end-module \ pixel

  \ Opening bracket for pixel{ ... }set/get
  ( tileset -- tileset )
  : pixel{ pixel :: params import [immediate] ;

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
    : (id>ram) 8 * VERA_SPRITE_RAM_BASE + ;

    \ Calculate the sprite id from the given sprite attribute RAM address.
    \ ( addr -- id )
    : (ram>id) VERA_SPRITE_RAM_BASE - 8 / ;

    : init ( sprite-idx sprite -- )
      over #SPRITES u< ?assert
      dup sprite-struct 0 fill ( sprite-idx sprite )
      swap (id>ram) ( sprite ramaddr )
      swap .attr-ram-ptr ! ( sprite )
    ;

    \ Retrieve the sprite id from the sprite object.
    : id@ ( sprite -- id ) .attr-ram-ptr @ (ram>id) ;

    \ Get the sprite's current coordinates.
    \ ( sprite -- vec2 )
    : xy@ dup .attr-x h@ swap .attr-y h@ vec2 ;

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

    ( size -- f )
    : (spritesize-is-valid?) l{ 8 , 16 , 32 , 64 }l find-in 0<> ;

    \ Set the sprite width
    \ ( width sprite -- )
    : (width!) 
      over (spritesize-is-valid?) ?assert
      swap (sizeenc) 
      swap .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH! 
    ;

    \ Set the sprite height
    \ ( height sprite -- )
    : (height!)
      over (spritesize-is-valid?) ?assert
      swap (sizeenc) swap .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT! 
    ;

    \ ( sprite -- flip )
    : flip@ .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP@ ;

    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS constant DIS \ Sprite disabled. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 constant BG_L0 \ Between background and L0. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 constant L0_L1 \ Between L0 and L1. 
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 constant L1 \ In front of L1. 

    \ ( sprite -- zdepth )
    : z@ .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH@ ;

    \ ( sprite -- colmask )
    : colmask@ .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK@ ;

    \ ( sprite -- paloffset )
    : paloffset@ .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET@ ;

    ( bpp -- f )
    : (bpp-is-valid?) l{ 4 , 8 }l find-in 0<> ;

    \ Set the sprite's BPP. 8 or 4.
    \ ( bpp sprite -- )
    : (bpp!) 
      over (bpp-is-valid?) ?assert
      swap 8 = swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE! 
    ;

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
    : tset@ .tileset @ ;

    \ Retrieve the tile-idx corresponding to this sprite.
    \ ( sprite -- tile-idx )
    : tidx@ .tile-idx @ ;

    ( sprite -- )
    : print
      >r 
      r@ colmask@ r@ z@ r@ flip@ r@ height@ r@ width@ r@ xy@ vec2.xy swap r@ id@
      s" sprite: %n id, %n x, %n y, %n w, %n h, %n flip, %n z, $%x colmask" printf cr
      r@ tidx@ r@ tset@ r@ addr@ r@ bpp@ r> paloffset@
      s" %n paloffset, %n bpp, $%x addr, $%x tset, %n tidx" printf cr
    ;

    begin-module params
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
      : flip over .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP! ;

      \ ( sprite zdepth -- sprite )
      : z over .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH! ;

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
          tileset :: tidx>addr ( sprite addr ) 
          over (addr!) ( sprite )
        else
          drop ( sprite )
        then
      ;
  
      \ Set the tileset to be used in the sprite object.
      \ When modifying the tileset used by a sprite object, keep in mind that
      \ the corresponding tile index (tidx, see above) has to be valid (within
      \ range) for the new tileset.
      : tset ( sprite tileset -- sprite )
        swap >r ( tileset R : sprite )
        r@ .tile-idx @ ( tileset tile-idx R: sprite )
        over tileset :: tidx>addr r@ (addr!) ( tileset R: sprite )
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
        params unimport
        [immediate]
      ;
    end-module \ sprite :: params

  end-module

  \ Create and initialize a sprite object.
  \ sprite-idx must be in range 0..NUM_SPRITES-1.
  \ ( sprite-idx "name" -- )
  : <sprite> 
    create here sprite :: sprite-struct allot ( sprite-idx sprite )
    sprite :: init ;

  \ Opening bracket for sprite{ ... }set
  ( sprite -- sprite )
  : sprite{ sprite :: params import [immediate] ;

  begin-module layer

    begin-structure layer-struct
      field:  .tileset
      field:  .tilemap
      hfield: .tile-idx
      cfield: .id
    end-structure

    \ Initialize a layer object
    : init ( id layer -- )
      over #LAYERS < ?assert ( id layer )
      dup layer-struct 0 fill ( id layer )
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
    : tmap-base@
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
    : tmap-width@ .id c@ if VERA_L1_CONFIG_MAP_WIDTH@ else VERA_L0_CONFIG_MAP_WIDTH@ then (sizedec) ;

      ( height layer-id -- )
    : (tilemap-height!)
      swap (sizeenc)
      swap if VERA_L1_CONFIG_MAP_HEIGHT! else VERA_L0_CONFIG_MAP_HEIGHT! then
    ;

      ( layer-id -- height )
    : tmap-height@
      .id c@
      if VERA_L1_CONFIG_MAP_HEIGHT@ else VERA_L0_CONFIG_MAP_HEIGHT@ then (sizedec) ;

      ( f layer-id -- )
    : (t256c!) if VERA_L1_CONFIG_T256C! else VERA_L0_CONFIG_T256C! then ;

      ( layer -- f )
    : t256c@ .id c@ if VERA_L1_CONFIG_T256C@ else VERA_L0_CONFIG_T256C@ then 0<> ;

    ( bpp - bpp-encoded )
    : (bppenc) log2 ;

    ( bpp-encoded -- bpp )
    : (bppdec) 1<< ;

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
    : (tile-width!) 
      if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH! else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH! then ;

    ( layer -- width-bit )
    : tile-width@ .id c@ if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@ else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@ then ;

    \ True sets tile height 16, false 8.
    \ ( f layer-id -- )
    : (tile-height!) if VERA_L1_TILEBASE_TILE_HEIGHT! else VERA_L0_TILEBASE_TILE_HEIGHT! then ;

    ( layer -- height )
    : tile-height@ .id c@ if VERA_L1_TILEBASE_TILE_HEIGHT@ else VERA_L0_TILEBASE_TILE_HEIGHT@ then ;

    ( addr layer-id -- )
    : (tile-base!)
      swap VERA_VRAM_BASE - 11 rshift ( layer-id addr )
      swap if VERA_L1_TILEBASE_TILE_BASEADDR! else VERA_L0_TILEBASE_TILE_BASEADDR! then
    ;

    ( layer -- addr-id )
    : tile-base@
      .id c@
      if VERA_L1_TILEBASE_TILE_BASEADDR@ else VERA_L0_TILEBASE_TILE_BASEADDR@ then
      11 lshift VERA_VRAM_BASE +
    ;

    ( size -- f )
    : (tilesize-is-valid?) l{ 8 , 16 }l find-in 0<> ;

    \ Configure given tileset into given layer.
    ( layer-id tileset -- )
    : (tileset!)
      [ tileset import ]
      >r ( layer-id R: tileset )
      r@ bpp@ over (bpp!) ( layer-id R: tileset )
      false over (bitmap-mode!) ( layer-id R: tileset )
      r@ width@ ( layer-id width R: tileset )
      dup (tilesize-is-valid?) ?assert
      16 = swap over (tile-width!) ( layer-id R: tileset )
      r@ height@ ( layer-id height R: tileset )
      dup (tilesize-is-valid?) ?assert
      16 = over (tile-height!) ( layer-id R: tileset )
      r> base@ dup ?assert ( layer-id base R: tileset )
      swap (tile-base!)
      [ tileset unimport ]
    ;

    ( size -- f )
    : (bitmap-width-is-valid?) l{ 320 , 640 }l find-in 0<> ;

    \ Configure given bitmap (identified by a bitmap descriptor) into the given layer.
    ( tileset tile-idx layer-id -- )
    : (bitmap!)
      [ tileset import ]
      >r ( tileset tile-idx R: layer-id )
      over tidx>addr r@ (tile-base!) ( tileset R: layer-id )
      dup bpp@ r@ (bpp!) ( tileset R: layer-id )
      width@ ( width R: layer-id )
      dup (bitmap-width-is-valid?) ?assert ( width R: layer-id )
      640 = r@ (tile-width!) ( f R: layer-id )
      0 r@ (tile-height!) ( R: layer-id )
      true r> (bitmap-mode!) ( )
      [ tileset unimport ]
    ;

    \ Retrieve tileset used by this layer
    ( layer -- tileset )
    : tset@ .tileset @ ;

    \ Retrieve tile-idx used by this layer (bitmap mode).
    ( layer -- tileset )
    : tidx@ .tile-idx h@ ;

    \ Retrieve tilemap used by this layer (tilemap mode).
    ( layer -- tilemap )
    : tmap@ .tileset @ ;

    ( layer -- )
    : print
      >r 
      r@ enabled? s" enabled: %n" printf cr
      r@ bitmap-mode@ if
        ." bitmap mode" cr
        r@ tidx@ r@ tset@ r@ tile-base@ r@ tile-height@ r@ tile-width@ r@ vscroll@ r@ hscroll@ r@ paloffset@ r> bpp@
        s" layer: %n bpp, %n paloffset, %n hscroll, %n vscroll, %n width, %n height, %n base, %n tset, %n tidx" 
        printf cr
      else
        ." tile mode" cr
        r@ tmap@ r@ tidx@ r@ tset@ r@ tile-base@ r@ tile-height@ r@ tile-width@ r@ vscroll@ r@ hscroll@ r@ bpp@
        s" layer: %n bpp, %n hscroll, %n vscroll, %n width, %n height, %n base, %n tset, %n tidx, $%x tilemap " printf cr
        r@ tmap@ r@ t256c@ tmap-height@ r@ tmap-width@ r@ tmap-base@
        s" layer: $x%x tmap-base, %n tmap-width, %n tmap-height, %n t256c" printf cr
      then
    ;

    begin-module params
      \ Set the tilemap to be used by this layer (configuring tilemapmode)
      ( layer map -- layer )
      : tmap over .tilemap ! ;

      \ Set the tileset to be used by this layer (tilemapmode and bitmapmode)
      ( layer tileset -- layer )
      : tset over .tileset ! ;
  
      \ Set the tile index to be used by this layer (bitmapmode)
      ( layer tile-idx -- layer )
      : tidx over .tile-idx h! ;

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
        params unimport
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
          rot (bitmap!)
        ;] compile-or-execute
        params unimport
        [immediate]
      ;
    end-module \ layer :: params 

  end-module \ layer

  \ l0 and l1 are the objects to be passed into the public words below.
  create l0 layer :: layer-struct allot
  create l1 layer :: layer-struct allot
  0 l0 layer :: init
  1 l1 layer :: init

  \ opening brack for layer{ ... }tilemap-mode or layer :: { ... }bitmap-mode
  ( layer -- layer )
  : layer{ layer :: params import [immediate] ;

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

    \ Shadow memory. VERA's palette memory is write-only.
    256 harray shadow_mem

    \ Expects standard 4-bit color fields mapped linearly
    \ Write an entry into the palette.
    \ @param idx: the palete color index
    \ @param rgb: the 12-bit RGB triple
    : write ( rgb idx -- )
      2dup shadow_mem h!
      swap ( idx rgb )
      $fff and ( idx rgbmasked )
      swap ( rgbmasked idx )
      4 * VERA_PALETTE_RAM_BASE + !
    ;

    \ Read the RGB value of a palette entry
    \ @param idx: the palete color index:
    \ @return: the 12-bit RGB triple
    : read ( idx -- rgb ) shadow_mem h@ ;

  end-module

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

