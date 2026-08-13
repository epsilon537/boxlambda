\ --- VERA_SPRITE_ATTR
\ --- 16-bit value bitfield accessors

\ -- Mask Offset
$fff #0 bitfield16@ VERA_SPRITE_ATTR_MODEADDR_ADDR@
$fff #0 bitfield16! VERA_SPRITE_ATTR_MODEADDR_ADDR!
$8000 #15 bitfield16@ VERA_SPRITE_ATTR_MODEADDR_MODE@
$8000 #15 bitfield16! VERA_SPRITE_ATTR_MODEADDR_MODE!

  #0 constant VERA_SPRITE_ATTR_MODEADDR_MODE_4BPP \ 4 bits per pixel sprite mode. 
  #1 constant VERA_SPRITE_ATTR_MODEADDR_MODE_8BPP \ 8 bits per pixel sprite mode. 


\ -- Mask Offset
$3ff #0 bitfield16@ VERA_SPRITE_ATTR_X@
$3ff #0 bitfield16! VERA_SPRITE_ATTR_X!

\ -- Mask Offset
$3ff #0 bitfield16@ VERA_SPRITE_ATTR_Y@
$3ff #0 bitfield16! VERA_SPRITE_ATTR_Y!

\ -- Mask Offset
$3 #0 bitfield16@ VERA_SPRITE_ATTR_FLAGS_FLIP@
$3 #0 bitfield16! VERA_SPRITE_ATTR_FLAGS_FLIP!

  #1 constant VERA_SPRITE_ATTR_FLAGS_FLIP_HFLIP \ Horizontal Flip 
  #2 constant VERA_SPRITE_ATTR_FLAGS_FLIP_VFLIP \ Vertical Flip 
  #3 constant VERA_SPRITE_ATTR_FLAGS_FLIP_HFLIP_VFLIP \ Horizonal and Vertical Flip 

$c #2 bitfield16@ VERA_SPRITE_ATTR_FLAGS_ZDEPTH@
$c #2 bitfield16! VERA_SPRITE_ATTR_FLAGS_ZDEPTH!

  #0 constant VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS \ Sprite disabled. 
  #1 constant VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 \ Between background and L0. 
  #2 constant VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 \ Between L0 and L1. 
  #3 constant VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 \ In front of L1. 

$f0 #4 bitfield16@ VERA_SPRITE_ATTR_FLAGS_COLMASK@
$f0 #4 bitfield16! VERA_SPRITE_ATTR_FLAGS_COLMASK!
$f00 #8 bitfield16@ VERA_SPRITE_ATTR_FLAGS_PALOFFSET@
$f00 #8 bitfield16! VERA_SPRITE_ATTR_FLAGS_PALOFFSET!
$3000 #12 bitfield16@ VERA_SPRITE_ATTR_FLAGS_WIDTH@
$3000 #12 bitfield16! VERA_SPRITE_ATTR_FLAGS_WIDTH!

  #0 constant VERA_SPRITE_ATTR_FLAGS_WIDTH_W8 \ 8 pixel sprite width 
  #1 constant VERA_SPRITE_ATTR_FLAGS_WIDTH_W16 \ 16 pixel sprite width 
  #2 constant VERA_SPRITE_ATTR_FLAGS_WIDTH_W32 \ 32 pixel sprite width 
  #3 constant VERA_SPRITE_ATTR_FLAGS_WIDTH_W64 \ 64 pixel sprite width 

$c000 #14 bitfield16@ VERA_SPRITE_ATTR_FLAGS_HEIGHT@
$c000 #14 bitfield16! VERA_SPRITE_ATTR_FLAGS_HEIGHT!

  #0 constant VERA_SPRITE_ATTR_FLAGS_HEIGHT_H8 \ 8 pixel sprite height 
  #1 constant VERA_SPRITE_ATTR_FLAGS_HEIGHT_H16 \ 16 pixel sprite height 
  #2 constant VERA_SPRITE_ATTR_FLAGS_HEIGHT_H32 \ 32 pixel sprite height 
  #3 constant VERA_SPRITE_ATTR_FLAGS_HEIGHT_H64 \ 64 pixel sprite height 

