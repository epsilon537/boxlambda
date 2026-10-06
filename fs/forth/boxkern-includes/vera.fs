\ BoxLambda Forth
\ VERA Graphics Driver

begin-module vera

  \ --- System Limits and Enumerations ---
  #479 constant SCANLINE-VISIBLE-MAX
  #524 constant SCANLINE-MAX
  #1023 constant HSTOP-MAX
  #1023 constant VSTOP-MAX
  #1024 constant MAX-TILES-IN-TILESET
  #2 constant #LAYERS
  #2 constant #SPRITE_BANKS
  #64 constant #SPRITES_IN_BANK
  #SPRITE_BANKS #SPRITES_IN_BANK * constant #SPRITES
  #SPRITES 1- constant MAX_SPRITE_ID
  #16 constant #PAL-GROUPS
  #16 constant #COLORS-IN-PAL-GROUP

  \ For setting the flip attribute of mapentries and sprites
  #2 constant VFLIP
  #1 constant HFLIP
  #3 constant VFLIP_HFLIP

  \ Used for parameter validation.
  \ Returns true if given mapentry or sprite flip value is valid.
  ( flip-value -- f )
  : (flip-is-valid?) l{ 0 , VFLIP , HFLIP , VFLIP_HFLIP }l find-in 0<> ;

  \ --- VRAM ---

  \ - VRAM internal helper Words.
  begin-module vram

    : x-alloc-failed ." VRAM allocation failed." cr ;

    \ This block size ensures that Vera alignment requirements
    \ are met when programming VRAM addresses in Vera registers.
    \ There is one case for bitmaps where more strict alignment is
    \ required. This is handled separately in the tilesize@ Word.
    #2048 constant BLOCK-SZ-BYTES
    BLOCK-SZ-BYTES log2 constant LOG-BLK-SZ
    VERA_VRAM_SIZE_BYTES LOG-BLK-SZ rshift constant #BLOCKS

    create blocks_ #BLOCKS chars allot

    \ Diagnostic print of # free and allocated blocks.
    ( -- )
    : usage
      0 ( allocated-blocks )
      #BLOCKS 0 do
        blocks_ i + c@ if
          1+
        then
      loop
      #BLOCKS over - ( allocated free )
      s" VRAM: free blocks: %n, allocated blocks: %n" printf cr
    ;

    \ In the blocks_ array, at offset, attempt to find requested consecutive blocks.
    \ Return actual # of blocks found (might be less than requested).
    ( requested offset -- found )
    : find-free-blocks
      [ 2 1 stack-checker ]
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
    ( #blocks -- block-idx|-1 )
    : find-free-chunk
      [ 1 1 stack-checker ]
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

    \ Allocate #blocks consecutive blocks starting at block-idx
    ( #blocks block-idx -- )
    : allocate-blocks
      [ 2 0 stack-checker ]
      blocks_ + ( #blocks vram_block_ptr )
      2dup c! ( #blocks vram_block_ptr )
      1+ swap 1- $ff fill ( )
    ;

    \ Find and allocate #blocks consecutive blocks in VRAM.
    ( #blocks -- block-idx )
    : find-alloc-blocks
      [ 1 1 stack-checker ]
      dup find-free-chunk ( #blocks block-idx )
      dup -1 = triggers x-alloc-failed
      tuck allocate-blocks ( block-idx )
    ;

    \ Release the given VRAM block.
    ( block-idx -- )
    : free-block
      [ 1 0 stack-checker ]
      blocks_ + ( vram_block_ptr )
      dup c@ ( vram_block_ptr #blocks )
      0 fill ( )
    ;
  end-module \ VRAM

  \ - Public VRAM API:

  \ Reset VRAM, release all VRAM resources.
  ( -- )
  : vram-reset
    vram :: blocks_ vram :: #BLOCKS 0 fill
    VERA_VRAM_BASE VERA_VRAM_SIZE_BYTES 0 fill 
  ;

  \ Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
  \ The sheet and tilemap creation/initialization Words use this Word to allocate their resources.
  \ size-bytes: the number of bytes to allocate.
  \ If successful, returns a 2KB-aligned Pointer to allocated block of memory in VRAM.
  \ In not successful an vram :: x-alloc-failed exception is raised.
  ( size-bytes -- addr )
  : vram-alloc
    [ 1 1 stack-checker ]
    dup 0= if exit then
    \ Convert size in bytes to block size, rounding up.
    [ vram :: BLOCK-SZ-BYTES 1- ] literal + vram :: LOG-BLK-SZ rshift ( #blocks )
    vram :: find-alloc-blocks ( block-idx )
    \ Convert to address
    vram :: LOG-BLK-SZ lshift VERA_VRAM_BASE +
  ;

  \ Release VRAM allocated with vram-alloc.
  ( addr -- )
  : vram-free
    [ 1 0 stack-checker ]
    \ Convert addr to block-idx
    VERA_VRAM_BASE - vram :: LOG-BLK-SZ rshift ( block-idx )
    vram :: free-block
  ;

  \ Return the VRAM base address.
  ( -- vram-base-addr )
  : vram-base VERA_VRAM_BASE ;

  \ Print VRAM usage.
  ( -- )
  : vram.
    vram :: usage
  ;

  \ --- Tile Maps

  \ Map types
  #0 constant TMAP-TXT16
  #1 constant TMAP-TXT256
  #2 constant TMAP-TILE

  \ - Tilemap internal helper Words.
  begin-module tilemap

    \ Tilemap object structure, including transient fields for the mapentry API.
    begin-structure tilemap-struct
      field:  .base
      field:  .position \ transient, used by mapentry
      hfield: .width
      hfield: .height
      hfield: .tidx \ transient, used by mapentry
      cfield: .type
      cfield: .fg \ transient, used by mapentry
      cfield: .bg \ transient, used by mapentry
      cfield: .pal-group \ transient, used by mapentry
      cfield: .flip \ transient, used by mapentry
    end-structure

    typechecker typecheck

    \ Initialize the tilemap object.
    ( tilemap -- )
    : init
      [ 1 0 stack-checker ]
      dup tilemap-struct 0 fill
      init-type typecheck
    ;

    \ Check if given position is within the tilemap's width/height boundaries.
    ( position tilemap -- f )
    : pos-in-range?
      [ 2 1 stack-checker ]
      typecheck
      >r
      vec2.xy ( x y R: tilemap )
      dup 0 >= swap r@ .height h@ < and ( x f R: sheet )
      swap dup 0 >= swap r> .width h@ < and ( f f )
      and
    ;

    \ Apply the tilemap parameters entered in a tmap{...}set block.
    ( tilemap -- )
    : apply
      [ 1 0 stack-checker ]
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
    ;
  end-module \ tilemap

  \ - Public Tilemap API:
  \
  \ A tilemap is a grid of tiles. The grid is characterized by width,
  \ height and tile type. The grid is populated used the mapentry{...} API.

  \ Tilemap parameters in a tmap{...}set/apply block.
  begin-module tmap-params
    tilemap import

    \ Keeps track of the tilemap object to which the attributes will be applied.
    0 variable tmap

    \ Used for parameter validation. Returns true if given width or height is valid.
    ( size -- f )
    : (size-is-valid?) l{ 32 , 64 , 128 , 256 }l find-in 0<> ;

    \ Set map width in the tilemap object: 32, 64, 128, 256.
    ( width -- )
    : width
      [ 1 0 stack-checker ]
      xassert{ dup (size-is-valid?) }xassert
      tmap @ .width h! 
    ;

    \ Set map height in the tilemap object: 32, 64, 128, 256.
    ( height -- )
    : height
      [ 1 0 stack-checker ]
      xassert{ dup (size-is-valid?) }xassert
      tmap @ .height h! 
    ;

    \ Used for validation purposes. Returns true if given type value is valid.
    ( type -- f )
    : (tmap-type-is-valid?) l{ TMAP-TXT16 , TMAP-TXT256 , TMAP-TILE }l find-in 0<> ;

    \ Set the map type : TXT16/TXT256/TILE.
    ( type -- )
    : type
      [ 1 0 stack-checker ]
      xassert{ dup (tmap-type-is-valid?) }xassert
      tmap @ .type c! ;

    \ (Re)Allocate VRAM for this tilemap to accommodate the width and height
    \ If VRAM was previously allocated for this tilemap,
    \ this VRAM will be released before reallocating VRAM.
    \ Throws vram :: x-alloc-failed exception if VRAM allocation failed.
    ( -- )
    : }apply
      [: 
        [ 0 0 stack-checker ]
        tmap @
        apply
      ;] compile-or-execute
      tmap-params unimport
      [immediate]
    ;

    \ Record the given tilemap parameters in the tilemap object, but don't apply
    \ the parameters yet. Useful when setting parameters piecemeal.
    ( -- )
    : }set
      tmap-params unimport
      [immediate]
    ;

    tilemap unimport
  end-module \ tmap-params

  \ Opening bracket for tmap{ ... }set/apply.
  ( tilemap -- )
  : tmap{
    [: tmap-params :: tmap ! ;] compile-or-execute
    tmap-params import 
    [immediate] ;

  \ Apply the tilemap parameters previously recorded in a tmap{...}set block.
  ( tilemap -- )
  : tmap-params-apply
    tilemap :: apply
  ;

  \ Returns the tilemap's width.
  ( tilemap -- width )
  : tmap-width@ 
    [ 1 1 stack-checker ]
    tilemap :: typecheck
    tilemap :: .width h@ ;

  \ Returns the tilemap's height.
  ( tilemap -- height )
  : tmap-height@ 
    [ 1 1 stack-checker ]
    tilemap :: typecheck
    tilemap :: .height h@ ;

  \ Returns the tilemap's type.
  ( tilemap -- type )
  : tmap-type@
    [ 1 1 stack-checker ]
    tilemap :: typecheck
    tilemap :: .type c@ ;

  \ Retruns the tilemap's base address.
  ( tilemap -- addr )
  : tmap-base@ 
    [ 1 1 stack-checker ]
    tilemap :: typecheck
    tilemap :: .base @ ;

  \ Deinitialize the tilemap, freeing VRAM resources.
  ( tilemap -- )
  : tmap-deinit
    [ 1 0 stack-checker ]
    tilemap :: typecheck
    dup tmap-base@ vram-free
    0 swap tilemap :: .base !
  ;

  \ Print the tilemap object attributes.
  ( tilemap -- )
  : tmap.
    [ 1 0 stack-checker ]
    tilemap :: typecheck
    >r r@ tmap-type@ r@ tmap-height@ r@ tmap-width@ r> tmap-base@
    s" Tilemap: $%x base, %n width, %n height, %n type" printf cr
  ;

  \ Create and initialize a tilemap object.
  ( "name" -- )
  : <tmap> 
    create here tilemap :: tilemap-struct allot tilemap :: init ;

  compileto-save
  compiletoimem

  \ --- Tilemap mapentry internal Words.
  begin-module mapentry

    \ Get the address of the entry at position in given map
    ( position tilemap -- addr )
    : position>addr
      [ tilemap import ]
      [ 2 1 stack-checker ]
      \ Calculate 2*(row*width_ + col)
      2dup tmap-width@ swap vec2.y * ( position map y*w )
      rot vec2.x + 2* ( map offset )
      swap tmap-base@ ( offset base )
      xassert{ dup }xassert
      +
      [ tilemap unimport ]
    ;

    \ Set mapentry at given position in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( mapentry position tilemap -- )
    : mapentry!
      [ 3 0 stack-checker ]
      position>addr h! ;

    \ Get mapentry at given in tilemap.
    \ position is a vec2, i.e. x first (column), then y (row).
    ( position tilemap -- mapentry )
    : mapentry@ 
      [ 2 1 stack-checker ]
      position>addr h@ ;

    \ Apply the mapentry parameters previously set in a
    \ mapentry{...}set block.
    ( tilemap -- )
    : mapentry-apply
      [ 1 0 stack-checker ]
      [ tilemap import ]
      typecheck
      dup .type c@ case 
        TMAP-TILE of  
          dup .pal-group c@ #12 lshift ( tmap mapentry )
          over .flip c@ 3 and #10 lshift or ( tmap mapentry )
          over .tidx h@ $3ff and or ( tmap mapentry )
        endof
        TMAP-TXT16 of
          dup .fg c@ $f and 8 lshift
          over .bg c@ $f and #12 lshift or
          over .tidx h@ $ff and or ( tmap mapentry )
        endof
        TMAP-TXT256 of
          dup .fg c@ $ff and 8 lshift
          over .tidx h@ $ff and or ( tmap mapentry )
        endof
        xassert{ false }xassert 0
      endcase
      over .position @ ( tmap mapentry position )
      rot ( mapentry position tmap )
      xassert{ 2dup tilemap :: pos-in-range? }xassert
      mapentry! ( )
      [ tilemap unimport ]
    ;
  end-module \ mapentry

  \ - Tilemap mapentry public API:

  \ Tilemap mapentry parameters in a tmap{...}set/apply block.
  begin-module mapentry-params
    \ mapentry :: { }set/get attributes
    tilemap import
    mapentry import

    \ Set mapentry background color.
    ( bg -- )
    : bg
      [ 1 0 stack-checker ]
      tmap-params :: tmap @ .bg c! ;

    \ Set mapentry foreground color.
    ( fg -- )
    : fg
      [ 1 0 stack-checker ]
      tmap-params :: tmap @ .fg c! ;

    \ Set mapentry tile index (character code).
    ( tile-idx -- )
    : tidx
      [ 1 0 stack-checker ]
      tmap-params :: tmap @ .tidx h! ;

    \ Set mapentry palette group. 0..15.
    ( pal-group -- )
    : pal-group
      [ 1 0 stack-checker ]
      tmap-params :: tmap @ .pal-group c! ;

    \ Set mapentry flip value: 0, VFLIP, HFLIP, or VFLIP_HFLIP
    ( flip -- )
    : flip
      [ 1 0 stack-checker ]
      xassert{ dup (flip-is-valid?) }xassert
      tmap-params :: tmap @ .flip c! ;

    \ Set mapentry xy position in the tilemap. The input parameter is a vec2 object (see vec2.fs).
    ( vec2 -- )
    : xy
      [ 1 0 stack-checker ]
      tmap-params :: tmap @ .position ! ;

    \ Apply the mapentry as specified in the ( tilemap ) mapentry{...}apply block.
    ( -- )
    : }apply
      [:
        [ 0 0 stack-checker ]
        tmap-params :: tmap @
        mapentry :: mapentry-apply
      ;] compile-or-execute
      mapentry-params unimport
      [immediate]
    ;

    \ Record the given mapentry parameters in the tilemap object, but don't apply
    \ the parameters yet. Useful when setting parameters piecemeal.
    ( -- )
    : }set
      mapentry-params unimport
      [immediate]
    ;

    \ Read from VRAM the mapentry specified by ( tilemap ) mapentry{ <vec2> xy }get and
    \ decode it, populating fg, bg, pal-group, flip attributes.
    \ This is useful for mapentry read-modify-write operations.
    ( -- )
    : }get
      [:
        [ tilemap import ]
        [ 0 0 stack-checker ]
        tmap-params :: tmap @ .position @ tmap-params :: tmap @ mapentry@ ( mapentry )
        xassert{ dup tmap-params :: tmap @ tilemap :: pos-in-range? }xassert
        tmap-params :: tmap @ tmap-type@ case 
          TMAP-TILE of
            dup #12 rshift tmap-params :: tmap @ .pal-group c! ( mapentry )
            dup #10 rshift 3 and tmap-params :: tmap @ .flip c! ( mapentry )
            $3ff and r@ .tidx h! ( )
          endof
          TMAP-TXT16 of
            dup 12 rshift tmap-params :: tmap @ .bg c! ( mapentry )
            dup 8 rshift $f and tmap-params :: tmap @ .fg c! ( mapentry )
            $ff and tmap-params :: tmap @ .tidx h! ( )
          endof
          TMAP-TXT256 of
            dup 8 rshift tmap-params :: tmap @ .fg c! ( mapentry )
            $ff and tmap-params :: tmap @ .tidx h! ( )
          endof
          xassert{ false }xassert
        endcase
        [ tilemap unimport ]
      ;] compile-or-execute
      mapentry-params unimport
      [immediate]
    ;

    tilemap unimport
    mapentry unimport
  end-module \ mapentry-params

  \ Opening bracket for ( tilemap ) mapentry{ ... }apply/set/get.
  ( tilemap -- )
  : mapentry{ 
    [: tmap-params :: tmap ! ;] compile-or-execute
    mapentry-params import 
    [immediate] 
  ;

  \ Apply the mapentry parameters previously recorded in a mapentry{...}set block.
  ( tilemap -- )
  : mapentry-params-apply
    mapentry :: mapentry-apply
  ;

  \ Set a 16-bit mapentry value at given position tilemap. 
  \ The position is specified by a vec2 object (see vec2.fs).
  ( mapentry vec2 tilemap -- )
  : mapentry!
   [ 3 0 stack-checker ]
    tilemap :: typecheck
    mapentry :: mapentry! 
  ;

  \ Read the 16-bit mapentry value from position in given tilemap
  \ The position is specified by a vec2 object (see vec2.fs).
  ( vec2 tilemap -- mapentry )
  : mapentry@
    [ 2 1 stack-checker ]
    tilemap :: typecheck
    mapentry :: mapentry@ 
  ;

  \ Keeping the 16-bit mapentry unpack words directly in the vera namespace for convenience:

  \ Unpack tidx, fg and bg color from a 16 color textmode map entry value.
  ( mapentry -- tidx fg bg )
  : unpack-txt16
    [ 1 3 stack-checker ]
    dup $ff and ( mapentry tidx )
    swap dup 8 rshift $f and ( tidx mapentry fg )
    swap 12 rshift $f and ( tidx fg bg )
  ;

  \ Unpack tidx and fg color from a 256 color textmode map entry value.
  ( mapentry -- tidx fg )
  : unpack-txt256
    [ 1 2 stack-checker ]
    dup $ff and ( mapentry tidx )
    swap 8 rshift $ff and ( tidx fg )
  ;

  \ Unpack tile, flip and palette group from a 2/4/8bpp tile map entry value.
  \ The color index of tile pixels is processed using the following logic:
  \ - Color indices 0 (transparent) and 16-255 are palette absolute.
  \ - Color indices 1-15 are relative to the palette group.
  ( mapentry -- tile-idx flip pal-group )
  : unpack-tile
    [ 1 3 stack-checker ]
    dup $3ff and ( mapentry tile-idx )
    swap 10 rshift 3 and ( tile-idx mapentry flip )
    swap 12 rshift $f and ( tile-idx flip pal-group )
  ;

  \ Internal Words for getting and setting pixels in tiles:
  begin-module pixel

    \ Calculate the byte pointer holding given position
    \ in 8bpp tile/bitmap of given width starting at
    \ given address.
    \ ( base width position -- ptr )
    : 8bpp-byte-ptr
      [ 3 1 stack-checker ]
      vec2.xy rot * + + ;

    \ Calculate the byte pointer holding given position
    \ in 4bpp tile/bitmap of given width starting at
    \ given address.
    \ ( base width position -- ptr )
    : 4bpp-byte-ptr
      [ 3 1 stack-checker ]
      vec2.xy rot * + 2/ + ;

    \ In a 4bpp tile/bitmap, calculate the bitoffset within a byte corresponding
    \ to the given vec2 position.
    \ ( position -- bitoffset )
    : 4bpp-x-bitoffset
      [ 1 1 stack-checker ]
      vec2.x
      1 dup rot \ 1 1 x
      and - \ 1-x&1
      2 lshift ;

    \ Calculate the byte pointer holding given position
    \ in 2bpp tile/bitmap of given width starting at
    \ given address.
    \ ( base width position -- ptr )
    : 2bpp-byte-ptr
      [ 3 1 stack-checker ]
      vec2.xy rot * + 4/ + ;

    \ In a 2bpp tile/bitmap, calculate the bitoffset within a byte corresponding
    \ to the given vec2 position.
    \ ( position -- bitoffset )
    : 2bpp-x-bitoffset
      [ 1 1 stack-checker ]
      vec2.x
      3 dup rot \ 3 3 x 
      and - \ 3-x&3 
      shl \ (3-x&3)*2
    ;

    \ Calculate the byte pointer holding given position
    \ in 1bpp tile/bitmap of given width starting at
    \ given address.
    \ ( base width position -- ptr )
    : 1bpp-byte-ptr
      [ 3 1 stack-checker ]
      vec2.xy rot * + 8/ + ;

    \ In a 1bpp tile/bitmap, calculate the bitoffset within a byte corresponding
    \ to the given vec2 position.
    \ ( position -- bitoffset )
    : 1bpp-x-bitoffset
      [ 1 1 stack-checker ]
      vec2.x 7 dup rot ( 7 7 x ) and - ( 7-x&7 ) ;

  \ -- Getting and settig pixels in various BPP modes:
  ( pxlval position base width -- )
  : 8bpp!
    [ 4 0 stack-checker ]
    rot pixel :: 8bpp-byte-ptr ( pxval ptr )
    c! ( )
  ;

  ( position base width -- pxlval )
  : 8bpp@
    [ 3 1 stack-checker ]
    rot pixel :: 8bpp-byte-ptr ( ptr )
    c@ ( pxlval )
  ;

  ( pxlval position base width -- )
  : 4bpp!
    [ 4 0 stack-checker ]
    rot dup pixel :: 4bpp-x-bitoffset >r ( pxlval base y width position R: bitoffset )
    pixel :: 4bpp-byte-ptr ( pxval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    $f r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
    rot $f and ( ptr oldbytemasked pxlvalmasked R: bitoffset )
    r> lshift or ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : 4bpp@
    [ 3 1 stack-checker ]
    rot dup pixel :: 4bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 4bpp-byte-ptr ( ptr R: bitoffset )
    c@ ( oldbyte R: bitoffset )
    r> rshift $f and ( pxlval )
  ;

  ( pxlval position base width -- )
  : 2bpp!
    [ 4 0 stack-checker ]
    rot dup pixel :: 2bpp-x-bitoffset >r ( pxlval base width position R: bitoffset )
    pixel :: 2bpp-byte-ptr ( pxval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    3 r@ lshift bic ( pxlval ptr oldbytemasked R: bitoffset )
    rot 3 and ( ptr oldbytemasked pxlvalmasked R: bitoffset )
    r> lshift or ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : 2bpp@
    [ 3 1 stack-checker ]
    rot dup pixel :: 2bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 2bpp-byte-ptr ( ptr R: bitoffset )
    c@ ( oldbyte R: bitoffset )
    r> rshift 3 and ( pxlval )
  ;

  ( pxlval position base width -- )
  : 1bpp!
    [ 4 0 stack-checker ]
    rot dup pixel :: 1bpp-x-bitoffset >r ( pxlval base width position R: bitoffset )
    pixel :: 1bpp-byte-ptr ( pxlval ptr R: bitoffset )
    dup c@ ( pxlval ptr oldbyte R: bitoffset )
    rot r> setbit ( ptr newbyte )
    swap c! ( )
  ;

  ( position base width -- pxlval )
  : 1bpp@
    [ 3 1 stack-checker ]
    rot dup pixel :: 1bpp-x-bitoffset >r ( base width position R: bitoffset )
    pixel :: 1bpp-byte-ptr ( ptr R: bitoffset )
    c@ r> rshift 1 and ( pxlval )
  ;
  end-module \ pixel

  compileto-restore

  \ Sheet types
  #0 constant SHEET-BITMAP
  #1 constant SHEET-TILESET

  \ --- Sheet internal Words.
  begin-module sheet

    \ The sheet object structure, including transient fields for the pxl{...} API.
    begin-structure sheet-struct
      field:  .base
      field:  .pxl-set
      field:  .pxl-get
      field:  .position \ transient, used by pxl{}
      field:  .bitmapaddr \ transient, used by pxl{}
      hfield: .width
      hfield: .height
      hfield: .bpp
      hfield: .#tiles \ Always 1 for bitmap sheets
      hfield: .tidx \ transient, used by pxl{}, always 0 in case of bitmaps.
      cfield: .color \ transient, used by pxl{}
      cfield: .type \ SHEET-BITMAP or SHEET-TILESET
    end-structure
    
    typechecker typecheck

    \ Initialize the sheet object.
    ( sheet type -- )
    : init
      [ 2 0 stack-checker ]
      over sheet-struct 0 fill ( sheet type )
      dup SHEET-BITMAP = if ( sheet type )
        over 1 swap .#tiles h!
      then ( sheet type )
      over .type c! ( sheet )
      init-type typecheck
    ;

    \ Returns the sheet type: SHEET-BITMAP or SHEET-TILESET.
    ( sheet --- type )
    : type@
      [ 1 1 stack-checker ]
      typecheck
      .type c@
    ;

    \ Retrieve the size in bytes one tile in the given tileset sheet.
    ( sheet -- size )
    : tilesize@ 
      [ 1 1 stack-checker ]
      typecheck
      >r
      xassert{ r@ type@ SHEET-TILESET = }xassert
      r@ .bpp h@ r@ .width h@ r> .height h@ * * 8/ ( sz )
    ;

    \ Retrieve the size in bytes of the given sheet.
    ( sheet -- size )
    : size@ 
      [ 1 1 stack-checker ]
      typecheck
      >r
      r@ type@ SHEET-BITMAP = if ( sz )
        r@ .bpp h@ r@ .width h@ r> .height h@ * * 8/ ( sz )
        \ Round up bitmaps to nearest higher multiple of $800 to meet tile-base address requirement
        $7ff + $fffff800 and
      else
        r@ .#tiles h@ r@ .bpp h@ r@ .width h@ r> .height h@ * * * 8/ ( sz )
      then
    ;

    \ Check if given position is within the width/height boundaries.
    ( position sheet -- f )
    : pos-in-range?
      [ 2 1 stack-checker ]
      >r
      vec2.xy ( x y R: sheet )
      dup 0 >= swap r@ .height h@ < and ( x f R: sheet )
      swap dup 0 >= swap r> .width h@ < and ( f f )
      and
    ;

    \ Applies the parameters configured in a sheet{...} block.
    ( sheet -- )
    : apply
      [ 1 0 stack-checker ]
      typecheck
      dup .base @ ?dup if
        vram-free
      then
      0 over .base ! ( sheet )
      dup size@ ( sheet sz )
      dup vram-alloc ( sheet sz addr )
      dup rot 0 fill ( sheet addr )
      swap .base ! ( )
    ;

    \ Given a tile index in a tileset sheet, compute the address (in VRAM) of the pixel data
    \ of that tile.
    \ @param tile_idx: Index of the tile in the sheet. Range 0..num_tiles-1.
    \ @param sheet: Sheet object
    ( tile-idx sheet -- addr )
    : tidx>addr
      [ 2 1 stack-checker ]
      typecheck
      dup tilesize@ ( tile-idx sheet tilesize )
      rot * ( sheet tilesize*tile-idx ) 
      swap .base @ ( tilesize*tile-idx base )
      xassert{ dup }xassert
      + ;

    \ Apply (draw) the pixel specified in a pxl{...} block.
    ( sheet -- )
    : apply-pxl
      [ 1 0 stack-checker ]
      typecheck
      >r
      r@ .color c@
      r@ .position @ xassert{ dup r@ pos-in-range? }xassert
      r@ type@ SHEET-BITMAP = if
        r@ .base @
      else
        r@ .tidx h@ xassert{ dup r@ .#tiles h@ <= }xassert
        r@ tidx>addr
      then
      r@ .width h@
      r> .pxl-set @
      ( color position addr width pxl-setter )
      execute
    ;
  end-module \ sheet

  \ -- Sheet Public API
  \ A sheet is used to represent tiles (e.g. a font), sprite pixel data, and bitmaps.

  \ Sheet parameters in a sheet{...}set/apply block.
  begin-module sheet-params
    sheet import

    0 variable (sheet)

    \ Used for parameter validation
    ( size -- f )
    : (tset-wh-is-valid?) l{ 8 , 16 , 32 , 64 }l find-in 0<> ;

    \ Used for parameter validation
    ( size -- f )
    : (bitmap-width-is-valid?) l{ 320 , 640 }l find-in 0<> ;

    \ Set the sheet width in the sheet object.
    \   - 8, 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - 320, 640 for bitmaps.
    ( width -- )
    : width
      [ 1 0 stack-checker ]
      xassert{ 
        (sheet) @ type@ SHEET-TILESET = if ( width )
          dup (tset-wh-is-valid?) ( width f )
        else
          dup (bitmap-width-is-valid?) ( width f )
        then
      }xassert
      (sheet) @ ( width sheet )
      .width h! 
    ;

    \ Set the sheet height in the sheet object
    \   - 8 or 16 for regular tiles.
    \   - 8, 16, 32, 64 for sprites.
    \   - 1..4095 for bitmaps.
    ( height -- )
    : height
      [ 1 0 stack-checker ]
      xassert{
        (sheet) @ type@ SHEET-TILESET = if ( height )
          dup (tset-wh-is-valid?) ( height f )
        else
          dup 0> ( height f ) 
          over 4096 < and ( height f )
        then
      }xassert
      (sheet) @ ( height sheet )
      .height h! 
    ;

    \ Set the sheet BPP in the sheet object
    \   - 1, 2, 4, 8 for regular tiles and bitmaps.
    \   - 4, 8 for sprites.
    ( bpp -- )
    : bpp
      [ 1 0 stack-checker ]
      dup case
        1 of pixel ::['] 1bpp! pixel ::['] 1bpp@ endof
        2 of pixel ::['] 2bpp! pixel ::['] 2bpp@ endof
        4 of pixel ::['] 4bpp! pixel ::['] 4bpp@ endof
        8 of pixel ::['] 8bpp! pixel ::['] 8bpp@ endof
        xassert{ false }xassert 0 0
      endcase ( bpp setter getter )
      (sheet) @ .pxl-get !
      (sheet) @ .pxl-set !
      (sheet) @ .bpp h!
    ;

    \ Set the number of tiles in the tilesheet.
    \ Range: 0..1023
    ( num -- )
    : tiles
      [ 1 0 stack-checker ]
      (sheet) @ ( num sheet )
      xassert{ 
        2dup type@ SHEET-TILESET = ( num sheet num f )
        swap 1024 < and ( num sheet f )
      }xassert ( num sheet )
      .#tiles h! 
    ;

    \ (Re)Allocate VRAM for this sheet to accommodate
    \ #tiles, bpp, width and height.
    \ If VRAM was previously allocated for this sheet,
    \ this VRAM will be released before reallocating VRAM.
    \ Throws x-alloc-failed exception if VRAM allocation failed.
    ( -- )
    : }apply
      [:
        [ 0 0 stack-checker ]
        (sheet) @
        apply
      ;] compile-or-execute
      sheet-params unimport
      [immediate]
    ;

    \ Store the parameters given in the sheet{...}set block, but don't apply
    \ them yet.
    ( -- )
    : }set
      sheet-params unimport
      [immediate]
    ;

    sheet unimport
  end-module \ sheet-params

  \ Opening bracket for sheet{ ... }set
  ( -- )
  : sheet{ 
    [: sheet-params :: (sheet) ! ;] compile-or-execute
    sheet-params import 
    [immediate] ;

  \ Apply the parameters previously configured in a sheet{...}
  \ Throws x-alloc-failed exception if VRAM allocation failed.
  ( sheet -- )
  : sheet-params-apply
    sheet :: apply
  ;

  \ Given a VRAM address and a tileset sheet, compute the tile index corresponding to that address.
  ( addr tileset -- tile-idx )
  : sheet-addr>tidx
    [ 2 1 stack-checker ]
    sheet :: typecheck
    dup sheet :: .base @ ( addr sheet baseaddr )
    xassert{ 
      2dup swap ( addr sheet baseaddr baseaddr sheet )
      sheet :: type@ SHEET-TILESET = and 
    }xassert
    rot swap - ( sheet offset )
    swap sheet :: tilesize@ ( offset tilesize )
    / ( tileidx )
  ;

  \ Given a tile index in a tileset sheet, compute the address (in VRAM) of the pixel data
  \ of that tile.
  \ tile_idx: Index of the tile in the sheet. Range 0..num_tiles-1.
  \ sheet: Sheet object
  ( tile-idx tileset -- addr )
  : sheet-tidx>addr
    sheet :: tidx>addr
  ;

  \ Returns the size in bytes of one tile in the given tileset. Tileset sheets only.
  ( sheet -- tilesize-bytes )
  : sheet-tilesize@ 
    [ 1 1 stack-checker ]
    sheet :: tilesize@ ;

  \ Returns the sheet size in bytes.
  ( sheet -- tilesize-bytes )
  : sheet-size@ 
    [ 1 1 stack-checker ]
    sheet :: size@ ;

  \ Returns the sheet width.
  ( sheet -- width )
  : sheet-width@ 
    [ 1 1 stack-checker ]
    sheet :: typecheck
    sheet :: .width h@ ;

  \ Retrieve the sheet height.
  ( sheet -- height )
  : sheet-height@ 
    [ 1 1 stack-checker ]
    sheet :: typecheck
    sheet :: .height h@ ;

  \ Returns the sheet base address in VRAM.
  ( sheet -- addr )
  : sheet-base@ 
    [ 1 1 stack-checker ]
    sheet :: typecheck
    sheet :: .base @ ;

  \ Retrieve the sheet's bits-per-pixel.
  ( sheet -- bpp )
  : sheet-bpp@ 
    [ 1 1 stack-checker ]
    sheet :: typecheck
    sheet :: .bpp h@ ;

  \ Retrieve the number of tiles in the sheet. Always returns 1 in case of a bitmap sheet.
  ( sheet -- #tiles )
  : sheet-#tiles@ 
    [ 1 1 stack-checker ]
    sheet :: typecheck
    sheet :: .#tiles h@ 
  ;

  \ Returns the sheet type: SHEET-BITMAP or SHEET-TILESET.
  ( sheet -- type )
  : sheet-type@
    sheet :: type@
  ;

  \ Print the sheet attributes.
  ( sheet -- )
  : sheet.
    [ 1 0 stack-checker ]
    sheet :: typecheck
    >r
    r@ sheet :: type@ SHEET-TILESET = if
      r@ sheet-#tiles@ r@ sheet-bpp@ r@ sheet-height@ r@ sheet-width@ r> sheet-base@
      s" Tileset Sheet: $%x base, %n width, %n height, %n bpp, %n tiles" printf cr
    else
      r@ sheet-bpp@ r@ sheet-height@ r@ sheet-width@ r> sheet-base@
      s" Bitmap Sheet: $%x base, %n width, %n height, %n bpp" printf cr
    then
  ;

  \ Create and initialize a bitmap sheet object.
  \ ( "name" -- )
  : <sheet-bitmap> create here sheet :: sheet-struct allot SHEET-BITMAP sheet :: init ;

  \ Create and initialize a tileset sheet object.
  \ ( "name" -- )
  : <sheet-tileset> create here sheet :: sheet-struct allot SHEET-TILESET sheet :: init ;
\
  \ Deinitialize the sheet, freeing VRAM resources.
  ( sheet -- )
  : sheet-deinit
    [ 1 0 stack-checker ]
    sheet :: typecheck
    dup sheet :: .base @ vram-free
    0 swap sheet :: .base !
  ;

  \ Pixel parameters in a <sheet> pxl{...}set/apply block.
  begin-module pxl-params
    sheet import
    sheet-params import

    \ tile_idx: Index of the tile in the tileset sheet. Range 0..num_tiles-1.
    ( tile-idx -- )
    : tidx
      [ 1 0 stack-checker ]
      xassert{ (sheet) @ sheet-type@ SHEET-TILESET = }xassert
      (sheet) @ .tidx h! 
    ;

    \ The pixel's color (palette index).
    ( color -- ) 
    : color
      [ 1 0 stack-checker ]
      (sheet) @ .color c! ;

    \ The pixel's position, specified as a vec2 (see vec2.fs).
    ( vec2 -- ) 
    : xy 
      [ 1 0 stack-checker ]
      (sheet) @ .position ! ;

    \ Draw the pixel as specified in the pxl{...}apply block.
    ( -- )
    : }apply
      [:
        [ 0 0 stack-checker ]
        (sheet) @
        apply-pxl
      ;] compile-or-execute
      pxl-params unimport
      [immediate]
    ;

    \ Store the pixel parameters specified in the pxl{...}set block, but don't
    \ apply them yet. Useful if some but not all parameters are known yet (e.g.
    \ in a loop, where the remaining parameters are specified inside the loop).
    ( -- )
    : }set
      pxl-params unimport
      [immediate]
    ;

    \ Read the pixel color from the position and tile given in the pxl{...}get block.
    \ ( -- color )
    : }get
      [:
        [ 0 1 stack-checker ]
        (sheet) @ .position @ xassert{ dup (sheet) @ sheet :: pos-in-range? }xassert
        (sheet) @ sheet-type@ SHEET-BITMAP = if
          (sheet) @ sheet-base@
        else
          (sheet) @ .tidx h@ xassert{ dup (sheet) @ sheet-#tiles@ <= }xassert
          (sheet) @ sheet-tidx>addr
        then
        (sheet) @ sheet-width@
        (sheet) @ sheet :: .pxl-get @
        ( position addr width pxl-getter )
        execute ( color )
        dup (sheet) @ .color c! ( color )
      ;] compile-or-execute
      pxl-params unimport
      [immediate]
    ;

    sheet-params unimport
    sheet unimport
  end-module \ pxl-params

  \ Opening bracket for pxl{ ... }set/get
  ( sheet -- )
  : pxl{ 
    [: sheet-params :: (sheet) ! ;] compile-or-execute
    pxl-params import [immediate] ;

  \ Apply the parameters previously specified in a pxl{...}set block.
  ( sheet -- )
  : pxl-params-apply
    sheet :: apply-pxl
  ;

  \ -- Sprites.

  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS constant SPR-DIS \ Sprite disabled.
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 constant SPR-BG-L0 \ Between background and L0.
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 constant SPR-L0-L1 \ Between L0 and L1.
  VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 constant SPR-L1 \ In front of L1.

  \ --- Sprite internal Words.
  begin-module sprite
    \ The sprite object structure.
    begin-structure sprite-struct
      field:  .sheet
      field:  .tile-idx
      field:  .attr-ram-ptr
      hfield: .attr-addr
      hfield: .attr-x
      hfield: .attr-y
      hfield: .attr-flags
    end-structure

    typechecker typecheck

    \ Calculate the sprite attribute RAM address from the given sprite id.
    ( id -- addr )
    : id>ram
      [ 1 1 stack-checker ]
      8 * VERA_SPRITE_RAM_BASE + ;

    \ Calculate the sprite id from the given sprite attribute RAM address.
    ( addr -- id )
    : ram>id
      [ 1 1 stack-checker ]
      VERA_SPRITE_RAM_BASE - 8 / ;

    : init ( sprite-idx sprite -- )
      [ 2 0 stack-checker ]
      xassert{ over #SPRITES u< }xassert
      dup sprite-struct 0 fill ( sprite-idx sprite )
      swap id>ram ( sprite ramaddr )
      over .attr-ram-ptr ! ( sprite )
      init-type typecheck
    ;

    \ Encode the sprite size to store in sprite attribute RAM
    ( tilesize - tilesize-encoded )
    : sizeenc
      [ 1 1 stack-checker ]
      log2 3 - ;

    \ Decode the sprite size stored in the sprite attribute RAM
    ( tilesize-encoded -- tilesize )
    : sizedec 
      [ 1 1 stack-checker ]
      3 + 1<< ;

    \ Returns true is given value is a valid sprite width or height.
    ( size -- f )
    : spritesize-is-valid? l{ 8 , 16 , 32 , 64 }l find-in 0<> ;

    \ Set the sprite width
    ( width sprite -- )
    : width! 
      [ 2 0 stack-checker ]
      xassert{ over spritesize-is-valid? }xassert
      swap sizeenc 
      swap .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH! 
    ;

    \ Set the sprite height
    ( height sprite -- )
    : height!
      [ 2 0 stack-checker ]
      xassert{ over spritesize-is-valid? }xassert
      swap sizeenc swap .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT! 
    ;

    \ Returns true is the given bits-per-pixel value is valid for sprites.
    ( bpp -- f )
    : bpp-is-valid? l{ 4 , 8 }l find-in 0<> ;

    \ Set the sprite's BPP. 8 or 4.
    ( bpp sprite -- )
    : bpp! 
      [ 2 0 stack-checker ]
      xassert{ over bpp-is-valid? }xassert
      swap 8 = swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE! 
    ;

    \ Set the sprite's VRAM address
    ( addr sprite -- )
    : addr!
      [ 2 0 stack-checker ]
      swap VERA_VRAM_BASE - 5 rshift ( sprite addr )
      swap .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR!
    ;

    \ Check if given position is within the boundaries
    ( position -- f )
    : pos-in-range?
      [ 1 1 stack-checker ]
      vec2.xy ( x y )
      dup 0 >= swap VSTOP-MAX <= and ( x f )
      swap dup 0 >= swap HSTOP-MAX <= and ( f f )
      and
    ;

    \ Apply the pameters specified in a spr{...}set/apply block.
    ( spr -- )
    : apply
      [ 1 0 stack-checker ]
      typecheck
      dup .attr-ram-ptr @ ( sprite attr-ram-addr )
      xassert{ dup }xassert ( sprite attr-ram-addr )
      swap .attr-addr ( attr-ram-addr attr-addr )
      2dup @ ( attr-ram-addr attr-addr attr-ram-addr attr0/1 ) 
      swap ! ( attr-ram-addr attr-addr )
      cell+ @ ( attr-ram-addr attr1/2 )
      swap cell+ ( attr1/2 attr-ram-addr' )
      !
    ;
    end-module

    \ Sprite parameters in a <spr> spr{...}set/apply block.
    begin-module spr-params
      sprite import

      0 variable spr

      \ Set the sprite position, specified by a vec2 (see vec2.fs).
      ( vec2 --)
      : xy 
          [ 1 0 stack-checker ]
          xassert{ dup sprite :: pos-in-range? }xassert
          vec2.xy ( x y )
          spr @ .attr-y h!
          spr @ .attr-x h!
      ;


      \ Set the sprite flip value: VFLIP, HFLIP, or VFLIP_HFLIP
      ( flip -- )
      : flip 
        [ 1 0 stack-checker ]
        xassert{ dup (flip-is-valid?) }xassert
        spr @ .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP! ;

      : (zdepth-is-valid?) l{ SPR-DIS , SPR-BG-L0 , SPR-L0-L1 , SPR-L1 }l find-in 0<> ;

      \ set the sprite z (depth) value: SPR-DIS, SPR-BG-L0, SPR-L0-L1, SPR-L1.
      ( zdepth -- )
      : z
        [ 1 0 stack-checker ]
        xassert{ dup (zdepth-is-valid?) }xassert
        spr @ .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH! ;

      \ Set the sprite collision mask.
      ( colmask -- )
        [ 1 0 stack-checker ]
      : colmask spr @ .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK! ;

      \ Set the sprite palette group.
      \ ( pal-group -- )
      : pal-group
        [ 1 0 stack-checker ]
        spr @ .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET! ;

      \ Set the tile index to be used by the sprite object. The tile index combined with the sheet object (:sheet below)
      \ identify the sprite pixel data.
      \ ( tile-idx -- )
      : tidx
        [ 1 0 stack-checker ]
        dup spr @ .tile-idx ! ( tile-idx )
        \ Compute and set the address attribute if we have a sheet.
        \ If we don't have a sheet yet, this is deferred until the sheet
        \ is specified.
        spr @ .sheet @ ?dup if ( tile-idx sheet ) xassert{ 2dup sheet-#tiles@ < }xassert ( tile-idx sheet )
          sheet-tidx>addr ( addr ) 
          spr @ addr! ( )
        else
          drop ( )
        then
      ;

      \ Set the tileset sheet to be used in the sprite object.
      \ When modifying the sheet used by a sprite object, keep in mind that
      \ the corresponding tile index (tidx, see above) has to be valid (within
      \ range) for the new sheet.
      : sheet ( sheet -- )
        [ 1 0 stack-checker ]
        sheet :: typecheck
        spr @ .tile-idx @ ( sheet tile-idx )
        xassert{ 2dup swap sheet-#tiles@ < }xassert ( sheet tile-idx )
        over sheet-tidx>addr spr @ addr! ( sheet )
        dup sheet-bpp@ spr @ bpp! ( sheet )
        dup sheet-width@ spr @ width! ( sheet )
        dup sheet-height@ spr @ height! ( sheet )
        spr @ .sheet ! ( )
      ;

      \ Commit the sprite's attributes to hardware, i.e. to the sprite attribute RAM.
      \ ( -- )
      : }apply
        [:
          [ 0 0 stack-checker ]
          spr @
          apply
        ;] compile-or-execute
        spr-params unimport
        [immediate]
      ;

      \ Store the sprite attributes specified in the spr{...}set block, but don't apply them to
      \ hardware yet.
      \ ( -- )
      : }set
        spr-params unimport
        [immediate]
      ;
      sprite unimport
    end-module \ spr-params

  \ Opening bracket for spr{ ... }set/apply.
  ( sprite -- sprite )
  : spr{ 
    [: spr-params :: spr ! ;] compile-or-execute
    spr-params import 
    [immediate] ;

  \ Apply (commit to hardware) the sprite attributes previously set in a spr{...}set block.
  ( sprite -- )
  : spr-params-apply
    sprite :: apply
  ;

  \ Returns the sprite's VRAM address.
  ( sprite -- addr )
  : spr-addr@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-addr VERA_SPRITE_ATTR_MODEADDR_ADDR@ 
    5 lshift VERA_VRAM_BASE +
  ;

  \ Returns the sprite's id.
  ( sprite -- id )
  : spr-id@
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-ram-ptr @ sprite :: ram>id ;

  \ Returns the sprite's current coordinates. Returns a vec2 (see vec2.fs).
  ( sprite -- vec2 )
  : spr-xy@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    dup sprite :: .attr-x h@ swap sprite :: .attr-y h@ vec2 ;

  \ Returns the sprite's width.
  ( sprite -- width )
  : spr-width@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_WIDTH@ sprite :: sizedec ;

  \ Reutrns the sprite's height.
  \ ( sprite -- height )
  : spr-height@
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_HEIGHT@ sprite :: sizedec ;

  \ Returns the sprite's flip value: VFLIP, HFLIP, or VFLIP_HFLIP
  \ ( sprite -- flip )
  : spr-flip@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_FLIP@ ;

  \ Returns the sprite's z-depth: SPR-DIS, SPR-BG-L0, SPR-L0-L1, SPR-L1.
  \ ( sprite -- zdepth )
  : spr-z@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_ZDEPTH@ ;

  \ Returns the sprite's collision mask.
  \ ( sprite -- colmask )
  : spr-colmask@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_COLMASK@ ;

  \ Returns the sprite's palette group.
  \ ( sprite -- pal-group )
  : spr-pal-group@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-flags VERA_SPRITE_ATTR_FLAGS_PALOFFSET@ ;

  \ Returns the sprite's bits-per-pixel value (8 or 4).
  \ ( sprite -- bpp )
  : spr-bpp@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-addr VERA_SPRITE_ATTR_MODEADDR_MODE@ if 8 else 4 then ;

  \ Returns the tileset sheet used by this sprite.
  \ ( sprite -- tileset )
  : spr-sheet@ 
    [ 1 1 stack-checker ]
    sprite :: typecheck
    sprite :: .sheet @ ;

  \ Retruns the tile index used by this sprite.
  \ ( sprite -- tile-idx )
  : spr-tidx@ 
    [ 1 1 stack-checker ]
    sprite :: .tile-idx @ ;

  \ Print the sprite's attributes.
  ( sprite -- )
  : spr.
    [ 1 0 stack-checker ]
    sprite :: typecheck
    >r 
    r@ spr-colmask@ r@ spr-z@ r@ spr-flip@ r@ spr-height@ r@ spr-width@ r@ spr-xy@ vec2.xy swap r@ spr-id@
    s" sprite: %n id, %n x, %n y, %n w, %n h, %n flip, %n z, $%x colmask" printf cr
    r@ spr-tidx@ r@ spr-addr@ r@ spr-bpp@ r> spr-pal-group@
    s" %n pal-group, %n bpp, $%x addr %n tidx" printf cr
  ;

  \ Create and initialize a sprite object.
  \ sprite-idx must be in range 0..NUM_SPRITES-1.
  \ ( sprite-idx "name" -- )
  : <spr> 
    [ 1 0 stack-checker ]
    create here sprite :: sprite-struct allot ( sprite-idx sprite )
    sprite :: init ;

  \ Reset the sprite attribute RAM for the given sprite object.
  ( sprite -- )
  : spr-deinit
    [ 1 0 stack-checker ]
    sprite :: typecheck
    sprite :: .attr-ram-ptr @ 8 0 fill
  ;

  \ --- Layer internal Words.
  begin-module layer

    \ The layer object structure.
    begin-structure layer-struct
      field:  .sheet
      field:  .tilemap
      cfield: .id
    end-structure

    typechecker typecheck

    \ Initialize a layer object
    : init ( id layer -- )
      [ 2 0 stack-checker ]
      xassert{ over #LAYERS < }xassert ( id layer )
      dup layer-struct 0 fill ( id layer )
      tuck .id c!
      init-type typecheck
    ;

    \ Set tilemap base address for the given layer
    \ ( addr layer-id -- )
    : tilemap-base!
      [ 2 0 stack-checker ]
      swap VERA_VRAM_BASE - 9 rshift ( layer-id vram-base )
      swap if VERA_L1_MAPBASE! else VERA_L0_MAPBASE! then
    ;

    \ Encode the map size to store in the layer config register.
    ( size - sizeencoded )
    : mapsizeenc log2 5 - ;

    \ Decode the map size stored in the layer config register.
    ( size - sizedecoded )
    : mapsizedec 5 + 1<< ;

    \ Set tilemap width for given layer.
    ( width layer-id -- )
    : tilemap-width!
      [ 2 0 stack-checker ]
      swap mapsizeenc
      swap if VERA_L1_CONFIG_MAP_WIDTH! else VERA_L0_CONFIG_MAP_WIDTH! then
    ;

    \ Set tilemap height for given layer.
    ( height layer-id -- )
    : tilemap-height!
      [ 2 0 stack-checker ]
      swap mapsizeenc
      swap if VERA_L1_CONFIG_MAP_HEIGHT! else VERA_L0_CONFIG_MAP_HEIGHT! then
    ;

    \ Enable/disable T256c mode.
      ( f layer-id -- )
    : t256c! 
      [ 2 0 stack-checker ]
      if VERA_L1_CONFIG_T256C! else VERA_L0_CONFIG_T256C! then ;

    \ Encode the bits-per-pixel setting to store in the layer config register.
    ( bpp - bpp-encoded )
    : bppenc 
      [ 1 1 stack-checker ]
      log2 ;

    \ Decode the bits-per-pixel setting stored in the layer config register.
    ( bpp-encoded -- bpp )
    : bppdec 
      [ 1 1 stack-checker ]
      1<< ;

    \ Set the layer's bits-per-pixel.
    ( bpp layer-id -- )
    : bpp!
      [ 2 0 stack-checker ]
      swap bppenc ( layer bpp-encoded )
      swap if VERA_L1_CONFIG_COLORDEPTH! else VERA_L0_CONFIG_COLORDEPTH! then
    ;

    \ Enable/disable bitmap mode.
    ( f layer-id -- )
    : bitmap-mode! 
      [ 2 0 stack-checker ]
      if VERA_L1_CONFIG_BITMAPMODE! else VERA_L0_CONFIG_BITMAPMODE! then ;

    \ In bitmap mode, true sets bitmap width 640, false 320.
    \ In tile mode, true sets tile width 16, false 8.
    \ ( f layer-id -- )
    : tile-width! 
      [ 2 0 stack-checker ]
      if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH! else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH! then ;

    \ True sets tile height 16, false 8.
    \ ( f layer-id -- )
    : tile-height! 
      [ 2 0 stack-checker ]
      if VERA_L1_TILEBASE_TILE_HEIGHT! else VERA_L0_TILEBASE_TILE_HEIGHT! then ;

    \ Set the tile base address.
    ( addr layer-id -- )
    : tile-base!
      [ 2 0 stack-checker ]
      swap VERA_VRAM_BASE - 11 rshift ( layer-id addr )
      swap if VERA_L1_TILEBASE_TILE_BASEADDR! else VERA_L0_TILEBASE_TILE_BASEADDR! then
    ;

    \ Reset the horizontal and vertical scroll value to 0.
    ( layer-id -- )
    : scroll-reset
      [ 1 0 stack-checker ]
      if 
        0 VERA_L1_HSCROLL_ADDR !
        0 VERA_L1_VSCROLL_ADDR !
      else 
        0 VERA_L0_HSCROLL_ADDR !
        0 VERA_L0_VSCROLL_ADDR !
      then
    ;

    \ Configure given tilemap into given layer.
    \ The tilemap attributes are used to configure the layer.
    ( tilemap layer-id -- )
    : tilemap!
      [ 2 0 stack-checker ]
      swap 
      tilemap :: typecheck
      xassert{ dup }xassert
      >r ( layer-id R: tilemap )
      r@ tmap-type@ TMAP-TXT256 = ( id 256c R: tilemap )
      over t256c! ( id R: tilemap )
      r@ tmap-width@ over tilemap-width! ( id R: tilemap )
      r@ tmap-height@ over tilemap-height! ( id R: tilemap )
      r> tmap-base@ xassert{ dup }xassert ( id tmap-base )
      swap tilemap-base! ( R: layer )
    ;

    \ Returns true if tile width/height value is valid. Used for parameter validation.
    ( size -- f )
    : tilewh-is-valid? l{ 8 , 16 }l find-in 0<> ;

    \ Configure given tileset sheet into given layer.
    \ The sheet attributes are used to configure the layer.
    ( sheet layer-id -- )
    : tileset!
      [ 2 0 stack-checker ]
      swap
      sheet :: typecheck
      xassert{ dup sheet-type@ SHEET-TILESET = }xassert ( sheet layer-id )
      >r ( layer-id R: sheet )
      \ Reset the scroll registers when installing a sheet
      \ to avoid side-effect from palette offset left over if we
      \ were previously in bitmap mode.
      dup scroll-reset ( layer-id R: sheet )
      r@ sheet-bpp@ over bpp! ( layer-id R: sheet )
      false over bitmap-mode! ( layer-id R: sheet )
      r@ sheet-width@ ( layer-id width R: sheet )
      xassert{ dup tilewh-is-valid? }xassert
      16 = over tile-width! ( layer-id R: sheet )
      r@ sheet-height@ ( layer-id height R: sheet )
      xassert{ dup tilewh-is-valid? }xassert
      16 = over tile-height! ( layer-id R: sheet )
      r> sheet-base@ xassert{ dup }xassert ( layer-id base )
      swap tile-base!
    ;

    \ Configure given bitmap (identified by a bitmap descriptor) into the given layer.
    \ The sheet attributes are used to configure the layer.
    ( sheet layer-id -- )
    : bitmap!
      [ 3 0 stack-checker ]
      >r ( sheet R: layer-id )
      \ Reset the scroll registers when installing a bitmap
      \ to avoid hscroll bleeding into palette offset if we
      \ were previously in tile mode.
      r@ scroll-reset ( sheet R: layer-id )
      sheet :: typecheck
      xassert{ dup sheet-type@ SHEET-BITMAP = }xassert ( sheet R: layer-id )
      dup sheet-base@ r@ tile-base! ( sheet R: layer-id )
      dup sheet-bpp@ r@ bpp! ( sheet R: layer-id )
      sheet-width@ 640 = r@ tile-width! ( f R: layer-id )
      0 r@ tile-height! ( R: layer-id )
      true r> bitmap-mode! ( )
    ;
  end-module \ layer

  \ l0 and l1 are the objects to be passed into the public words below.
  create l0 layer :: layer-struct allot
  create l1 layer :: layer-struct allot
  0 l0 layer :: init
  1 l1 layer :: init

  \ Returns layer object's the layer id (0 or 1).
  \ ( layer -- id )
  : layer-id@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@
  ;
 
  \ Configure the layer in tilemap mode.
  ( tmap tileset layer -- )
  : layer-tilemap-mode
    [ 3 0 stack-checker ]
    layer :: typecheck
    layer-id@ >r ( tmap sheet R: layer-id )
    swap r@ layer :: tilemap! ( sheet R: layer-id )
    r> layer :: tileset!
  ;

  \ Configure the layer in bitmap mode.
  ( bitmap layer -- )
  : layer-bitmap-mode
    [ 2 0 stack-checker ]
    layer :: typecheck
    xassert{ over }xassert
    layer-id@ ( bitmap layer-id )
    layer :: bitmap!
  ;

  \ Enable/disable the layer.
  \ ( f layer -- )
  : layer-enable
    [ 2 0 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_DC_VIDEO_L1_ENABLE! else VERA_DC_VIDEO_L0_ENABLE! then ;

  \ Returns true if the layer is enabled.
  ( layer -- f )
  : layer-enabled?
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_DC_VIDEO_L1_ENABLE@ else VERA_DC_VIDEO_L0_ENABLE@ then 0<> ;

  \ Returns the layer's tilemap base address.
  ( layer -- addr )
  : layer-tmap-base@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_MAPBASE@ else VERA_L0_MAPBASE@ then
    9 lshift VERA_VRAM_BASE +
  ;

  \ Returns the layer's tilemap width.
  ( layer -- width )
  : layer-tmap-width@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_MAP_WIDTH@ else VERA_L0_CONFIG_MAP_WIDTH@ then layer :: mapsizedec ;

  \ Retrieve the layer's tilemap height.
  ( layer -- height )
  : layer-tmap-height@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_CONFIG_MAP_HEIGHT@ else VERA_L0_CONFIG_MAP_HEIGHT@ then layer :: mapsizedec ;

  \ Returns true if the layer is in T256c mode.
  ( layer -- f )
  : layer-t256c@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_T256C@ else VERA_L0_CONFIG_T256C@ then 0<> ;

  \ Retrieve the layer's bits-per-pixel.
  ( layer -- bpp )
  : layer-bpp@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ ( id )
    if VERA_L1_CONFIG_COLORDEPTH@ else VERA_L0_CONFIG_COLORDEPTH@ then
    layer :: bppdec
  ;

  \ Returns true if the layer is in bitmap mode.
  ( layer -- f )
  : layer-bitmap-mode@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_CONFIG_BITMAPMODE@ else VERA_L0_CONFIG_BITMAPMODE@ then 0<> ;

  \ Returns the layer's palette group (assumes bitmap mode).
  ( layer -- pal-group )
  : layer-pal-group@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET@ else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET@ then ;

  \ Set the palette group to be used by this layer (bitmap mode).
  ( pal-group layer -- )
  : layer-pal-group! 
    [ 2 0 stack-checker ]
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET! else VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET! then
  ;

  \ Set the layer's horizontal scroll value.
  ( hscroll layer -- )
  : layer-hscroll!
    [ 2 0 stack-checker ]
    layer :: typecheck
    dup layer :: .id c@ ( hscroll layer id )
    swap layer-bitmap-mode@ if ( hscroll id )
      if VERA_L1_HSCROLL_HSCROLL_7_0! else VERA_L0_HSCROLL_HSCROLL_7_0! then
    else ( hscroll id )
      if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( hscroll addr )
      !
    then
  ;

  \ Returns the layer's horizontal scroll value.
  ( layer -- hscroll )
  : layer-hscroll@
    [ 1 1 stack-checker ]
    layer :: typecheck
    dup layer :: .id c@ ( layer id )
    swap layer-bitmap-mode@ if ( id )
      if VERA_L1_HSCROLL_HSCROLL_7_0@ else VERA_L0_HSCROLL_HSCROLL_7_0@ then
    else ( id )
      if VERA_L1_HSCROLL_ADDR else VERA_L0_HSCROLL_ADDR then ( addr )
      @
    then
  ;

  \ Set the layer's vertical value.
  ( vscroll layer -- )
  : layer-vscroll! 
    [ 2 0 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_VSCROLL! else VERA_L0_VSCROLL! then
  ;

  \ Retrieve the layer's vertical scroll valye.
  ( layer -- vscroll )
  : layer-vscroll@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@ if VERA_L1_VSCROLL@ else VERA_L0_VSCROLL@ then
  ;

  \ Returns the layer's tile or bitmap width.
  ( layer -- width )
  : layer-width@
    [ 1 1 stack-checker ]
    layer :: typecheck
    dup layer :: .id c@ 
    if VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@ else VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@ then ( layer w )
    1+ ( layer w=1|2 )
    swap layer-bitmap-mode@ if 320 else 8 then *
  ;

  \ Retruns the layer's tile height. Returns 0 if the layer is in bitmap mode.
  ( layer -- height )
  : layer-tile-height@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    dup layer-bitmap-mode@ if ( layer )
      drop 0
    else
      layer :: .id c@ if VERA_L1_TILEBASE_TILE_HEIGHT@ else VERA_L0_TILEBASE_TILE_HEIGHT@ then
      1+ 8 *
    then
  ;

  \ Returns the layer's VRAM base address.
  ( layer -- addr-id )
  : layer-base@
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .id c@
    if VERA_L1_TILEBASE_TILE_BASEADDR@ else VERA_L0_TILEBASE_TILE_BASEADDR@ then
    11 lshift VERA_VRAM_BASE +
  ;

  \ Returns sheet object used by this layer
  ( layer -- sheet )
  : layer-sheet@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .sheet @ ;

  \ Returns tilemap object used by this layer (tilemap mode).
  ( layer -- tilemap )
  : layer-tmap@ 
    [ 1 1 stack-checker ]
    layer :: typecheck
    layer :: .tilemap @ ;

  \ Print the layer attributes.
  ( layer -- )
  : layer.
    layer :: typecheck
    [ 1 0 stack-checker ]
    >r
    r@ layer-enabled? if s" enabled" else s" disabled" then
    r@ layer-id@
    s" layer %n %s" printf cr
    r@ layer-bitmap-mode@ if
      ." bitmap mode" cr
      r@ layer-base@ r@ layer-width@ 
      r@ layer-vscroll@ r@ layer-hscroll@ r@ layer-pal-group@ r> layer-bpp@
      s" %n bpp, %n pal-group, %n hscroll, %n vscroll, %n width, $%x base" 
      printf cr
    else
      ." tile mode" cr
      r@ layer-base@ r@ layer-tile-height@ r@ layer-width@ 
      r@ layer-vscroll@ r@ layer-hscroll@ r@ layer-bpp@
      s" %n bpp, %n hscroll, %n vscroll, %n width, %n height, $%x base," printf cr
      r@ layer-t256c@ r@ layer-tmap-height@ r@ layer-tmap-width@ r> layer-tmap-base@
      s" $%x tmap-base, %n tmap-width, %n tmap-height, %n t256c" printf cr
    then
  ;

  \ Enable/disable VGA line capture.
  ( f -- )
  : line-capture-enable
    [ 1 0 stack-checker ]
    VERA_CTRL_STATUS_CAPTURE_EN! ;

  \ Returns true if VGA line capture is pending. Returns false if line capture has been completed.
  ( -- f )
  : line-capture-enabled?
    [ 0 1 stack-checker ]
    VERA_CTRL_STATUS_CAPTURE_EN@ 0<> ;

  \ Read the RGB value of a pixel on the captured line.
  \ @param x: the pixel's x position. Range: 0..639.
  \ @return: 12-bit RGB triple.
  : line-capture-pxl@ ( x -- rgb ) 
    [ 1 1 stack-checker ]
    4 * VERA_CAPTURE_RAM_BASE + @ $fff and ;

  \ --- Interrupt Subsystem ---

  VERA_IEN_VAL_VSYNC constant IRQ-VSYNC-MASK
  VERA_IEN_VAL_LINE constant IRQ-LINE-MASK
  VERA_IEN_VAL_SPRCOL constant IRQ-SPRCOL-MASK

  \ Enable IRQs. The passed in mask will be OR'd with the installed mask.
  \ @param mask: bitwise OR of VERA_IRQs to enable.
  ( mask -- )
  : irq-enable
    [ 1 0 stack-checker ]
    VERA_IEN_ADDR @ or VERA_IEN_ADDR ! ;

  \ Disable IRQs. The passed in mask will be inverted and  AND'd with the
  \ installed mask.
  \ mask: bitwise OR of VERA_IRQs to disable.
  ( mask -- ) 
  : irq-disable
    [ 1 0 stack-checker ]
    VERA_IEN_ADDR @ swap bic VERA_IEN_ADDR ! ;

  \ Returns the enabled IRQs bitmask.
  ( -- mask )
  : irq-enabled
    [ 0 1 stack-checker ]
    VERA_IEN_ADDR @ ;

  \ Returns a bitmask of active VERA_IRQs.
  ( -- active-mask )
  : irq-get
    [ 0 1 stack-checker ]
    VERA_ISR_ADDR @ VERA_IEN_ADDR @ and ;

  \ Acknowledge IRQs.
  \ @param mask: bitwise OR of VERA_IRQs to acknowledge.
  ( mask -- )
  : irq-ack
    [ 1 0 stack-checker ]
    VERA_ISR_ISR! ;

  \ Set the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
  \ enabled.
  \ @param scanline: scanline number on which the trigger the line IRQ, must be
  \ <= VERA_SCANLINE_MAX.
  ( scanline -- )
  : irqline!
    [ 1 0 stack-checker ]
    VERA_IRQLINE! ;

  \ Returns the line IRQ's scanline value.
  ( -- scanline )
  : irqline@
    [ 0 1 stack-checker ]
    VERA_IRQLINE@ ;

  \ Retruns the current VGA scanline value.
  : scanline@ ( -- scanline ) 
    [ 0 1 stack-checker ]
    VERA_SCANLINE@ ;

  \ --- Palette API

  \ --- Palette Internal Words
  begin-module palette
    \ Shadow memory. VERA's palette memory is write-only.
    create shadow-palette 2 256 * allot
 
  \ Internal Word used by pal! and pal-init.
  ( rgb idx -- )
  : pal!
    swap ( idx rgb )
    $fff and ( idx rgbmasked )
    swap ( rgbmasked idx )
    4 * VERA_PALETTE_RAM_BASE + !
  ;

  end-module

  \ Paletter Group 0 Color Palette Indices
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
  #10 constant LIGHT-RED
  #11 constant DARK-GREY
  #12 constant GREY
  #13 constant LIGHT-GREEN
  #14 constant LIGHT-BLUE
  #15 constant LIGHT-GREY
  #16 constant GREYSCALE-0 
  #31 constant GREYSCALE-15 

  \ Palette Group 1 - Grey scale equivalent of the colors in Palette Group 0
  \ (in default VERA color palette).

  \ Given a Palette Group 0 color index, returns the corresponding greyscale
  \ color palette index.
  ( n -- n' )
  : greyscale #15 and GREYSCALE-0 + [1-foldable] ;

  \ Write an entry into the palette.
  \ idx: the palete color index (0..255).
  \ rgb: the 12-bit RGB triple.
  ( rgb idx -- )
  : pal!
    [ 2 0 stack-checker ]
    2dup 2* palette :: shadow-palette + h!
    palette :: pal!
  ;

  \ Read the RGB value of a palette entry.
  \ idx: the palete color index.
  \ returns the 12-bit RGB triple.
  ( idx -- rgb )
  : pal@
    [ 1 1 stack-checker ]
    2* palette :: shadow-palette + h@ ;

  \ Convert given palette group id and relative index (0..15) to its absolute 
  \ color palette index value (0..255).
  ( pal-group-id rel-idx -- abs-idx )
  : pal-group>pal-abs 
    [ 2 1 stack-checker ]
    swap 4 lshift or ;

  \ Convert a color palette absolute index (0..255) to the palette group it belongs to and 
  \ the relative index within this group.
  ( abs-idx -- pal-group-id rel-idx )
  : pal-abs>pal-group
    [ 1 2 stack-checker ]
    dup 4 rshift swap $f and ;
  ;

  \ Set all 16 rgb colors in the given palette group. Note palette group id on top-of-stack.
  ( rgb0 .. rgb15 pal-group-id -- )
  : pal-group!
    [ 17 0 stack-checker ]
    0 pal-group>pal-abs ( rgb0 .. rgb15 abs-idx )
    dup 15 + ( rgb0 .. rgb15 start-idx end-idx )
    do
      i pal!
    -1 +loop
  ;

  \ Set one of the 16 colors in the given palette group.
  ( rgb pal-group-id idx -- )
  : pal-group-1!
    [ 3 0 stack-checker ]
    pal-group>pal-abs pal!
  ;

  \ Retrieve all 16 rgb color from given palette group.
  ( pal-group-id -- rgb0 .. rgb15 )
  : pal-group@
    [ 1 16 stack-checker ]
    0 pal-group>pal-abs ( abs-idx )
    dup 16 + swap ( end-idx start-idx )
    do
      i pal@
    loop
  ;

  \ Retrieve the rgb color value from given relative index (0..15) in given palette group.
  ( pal-group-id idx -- rgb )
  : pal-group-1@
    [ 2 1 stack-checker ]
    pal-group>pal-abs pal@
  ;

  \ Load the original into the shadow-palette and VERA's palette memory.
  ( -- )
  : pal-init
    (orig-palette) palette :: shadow-palette 2 256 * move
    256 0 do
      i pal@ i palette :: pal!
    loop
  ;

  \ Load a palette into VERA palette memory.
  \ addr points to a block of 256 half-words, each half-word specifying a 12-bit rgb value
  \ corresponding to its index.
  ( addr -- )
  : pal-load
    [ 1 0 stack-checker ]
    move palette :: shadow-palette 512 \ Copy it to the shadow-palette first
    pal-init \ Then install shadow-palette into VERA palette memory.
  ;

  \ -- VERA top-level definitions

  \ Enable/disable the display.
  ( flag -- )
  : display-enable
    [ 1 0 stack-checker ]
    if 1 else 0 then VERA_DC_VIDEO_OUTPUT_MODE! ;

  \ Returns true if the display is enabled.
  ( -- flag )
  : display-enabled?
    [ 0 1 stack-checker ]
    VERA_DC_VIDEO_OUTPUT_MODE@ 0<> ;

  \ Enable/disable sprite rendering.
  ( flag -- )
  : sprites-enable
    [ 1 0 stack-checker ]
    VERA_DC_VIDEO_SPR_ENABLE! ;

  \ Returns true is sprite rendering is enabled.
  ( -- flag )
  : sprites-enabled?
    [ 0 1 stack-checker ]
    VERA_DC_VIDEO_SPR_ENABLE@ 0<> ;

  \ Set the horizontal scaling value. The passed in value is a unsigned fixed point 1.7 value.
  ( scale-ufix1-7 -- )
  : hscale!
    [ 1 0 stack-checker ]
    VERA_DC_HSCALE! ;

  \ Retrieve the horizontal scaling value (unsigned fixed point 1.7 value).
  ( -- scale-ufix1-7 )
  : hscale@
    [ 0 1 stack-checker ]
    VERA_DC_HSCALE@ ;

  
  \ Set the vertical scaling value. The passed in value is a unsigned fixed point 1.7 value.
  ( scale-ufix1-7 -- )
  : vscale!
    [ 1 0 stack-checker ]
    VERA_DC_VSCALE! 
    ;

  \ Retrieve the vertical scaling value (unsigned fixed point 1.7 value).
  ( -- scale-ufix1-7 )
  : vscale@
    [ 0 1 stack-checker ]
    VERA_DC_VSCALE@ ;

  \ Set the border color (palette index).
  ( pal-idx -- )
  : bordercolor!
    [ 1 0 stack-checker ]
    VERA_DC_BORDERCOLOR! ;

  \ Retrieve the border color palette index.
  ( -- pal-idx ) 
  : bordercolor@
    [ 0 1 stack-checker ]
    VERA_DC_BORDERCOLOR@ ;

  \ Set screen boundaries.
  ( hstart hstop vstart vstop -- )
  : boundaries!
    [ 4 0 stack-checker ]
    VERA_DC_VSTOP! VERA_DC_VSTART! VERA_DC_HSTOP! VERA_DC_HSTART! ;

  \ Get screen boundaries
  ( -- hstart hstop vstart vstop )
  : boundaries@
    [ 0 4 stack-checker ]
    VERA_DC_HSTART@ VERA_DC_HSTOP@ VERA_DC_VSTART@ VERA_DC_VSTOP@ ;

  \ Select the sprite bank to use.
  ( 1|0 -- )
  : sprite-bank! 
    [ 1 0 stack-checker ]
    VERA_CTRL_STATUS_SBNK! ;

  \ Get the selected sprite bank.
  ( -- 1|0 )
  : sprite-bank@ 
    [ 0 1 stack-checker ]
    VERA_CTRL_STATUS_SBNK@ ;

  \ Reset the sprite attribute RAM and reset sprite bank to 0.
  ( -- )
  : sprite-reset
    VERA_SPRITE_RAM_BASE #SPRITES 8 * 0 fill 
    0 sprite-bank!
  ;

  \ Initialize the Vera subsystem.
  ( -- )
  : vera-init
    vram-reset
    pal-init
    sprite-reset
  ;

end-module

