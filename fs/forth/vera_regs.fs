\ --- VERA
\ --- 32-bit register bitfield accessors

$12000000 constant VERA_BASE_ADDR


\ CTRL_STATUS - Control/Status register.
VERA_BASE_ADDR $0 + constant VERA_CTRL_STATUS_ADDR

\ -- Address Mask Offset
VERA_CTRL_STATUS_ADDR $1 #0 bitfield@ VERA_CTRL_STATUS_SBNK@
VERA_CTRL_STATUS_ADDR $1 #0 bitfield! VERA_CTRL_STATUS_SBNK!
VERA_CTRL_STATUS_ADDR $2 #1 bitfield@ VERA_CTRL_STATUS_CAPTURE_EN@
VERA_CTRL_STATUS_ADDR $2 #1 bitfield! VERA_CTRL_STATUS_CAPTURE_EN!

\ DC_BORDERCOLOR - Display composer border color register.
VERA_BASE_ADDR $4 + constant VERA_DC_BORDERCOLOR_ADDR

\ -- Address Mask Offset
VERA_DC_BORDERCOLOR_ADDR $ff #0 bitfield@ VERA_DC_BORDERCOLOR@
VERA_DC_BORDERCOLOR_ADDR $ff #0 bitfield! VERA_DC_BORDERCOLOR!

\ IEN - Interrupt enable register.
VERA_BASE_ADDR $8 + constant VERA_IEN_ADDR

\ -- Address Mask Offset
VERA_IEN_ADDR $7 #0 bitfield@ VERA_IEN@
VERA_IEN_ADDR $7 #0 bitfield! VERA_IEN!

  #1 constant VERA_IEN_VAL_VSYNC \ Vertical sync interrupt enable. 
  #2 constant VERA_IEN_VAL_LINE \ Line interrupt enable. 
  #4 constant VERA_IEN_VAL_SPRCOL \ Sprite collision interrupt enable. 


\ ISR - Interrupt status register.
VERA_BASE_ADDR $c + constant VERA_ISR_ADDR

\ -- Address Mask Offset
VERA_ISR_ADDR $7 #0 bitfield@ VERA_ISR_ISR@
VERA_ISR_ADDR $7 #0 bitfield! VERA_ISR_ISR!
VERA_ISR_ADDR $f0 #4 bitfield@ VERA_ISR_SPR_COLLISIONS@

\ IRQLINE - Interrupt line register.
VERA_BASE_ADDR $10 + constant VERA_IRQLINE_ADDR

\ -- Address Mask Offset
VERA_IRQLINE_ADDR $3ff #0 bitfield@ VERA_IRQLINE@
VERA_IRQLINE_ADDR $3ff #0 bitfield! VERA_IRQLINE!

\ SCANLINE - Scanline register
VERA_BASE_ADDR $14 + constant VERA_SCANLINE_ADDR

\ -- Address Mask Offset
VERA_SCANLINE_ADDR $3ff #0 bitfield@ VERA_SCANLINE@

\ DC_VIDEO - Display composer video register.
VERA_BASE_ADDR $18 + constant VERA_DC_VIDEO_ADDR

\ -- Address Mask Offset
VERA_DC_VIDEO_ADDR $3 #0 bitfield@ VERA_DC_VIDEO_OUTPUT_MODE@
VERA_DC_VIDEO_ADDR $3 #0 bitfield! VERA_DC_VIDEO_OUTPUT_MODE!

  #0 constant VERA_DC_VIDEO_OUTPUT_MODE_DIS \ Video disabled. 
  #1 constant VERA_DC_VIDEO_OUTPUT_MODE_VGA \ VGA output enabled. 

VERA_DC_VIDEO_ADDR $10 #4 bitfield@ VERA_DC_VIDEO_L0_ENABLE@
VERA_DC_VIDEO_ADDR $10 #4 bitfield! VERA_DC_VIDEO_L0_ENABLE!
VERA_DC_VIDEO_ADDR $20 #5 bitfield@ VERA_DC_VIDEO_L1_ENABLE@
VERA_DC_VIDEO_ADDR $20 #5 bitfield! VERA_DC_VIDEO_L1_ENABLE!
VERA_DC_VIDEO_ADDR $40 #6 bitfield@ VERA_DC_VIDEO_SPR_ENABLE@
VERA_DC_VIDEO_ADDR $40 #6 bitfield! VERA_DC_VIDEO_SPR_ENABLE!

\ DC_HSCALE - Display composer horizontal scale register.
VERA_BASE_ADDR $20 + constant VERA_DC_HSCALE_ADDR

\ -- Address Mask Offset
VERA_DC_HSCALE_ADDR $ff #0 bitfield@ VERA_DC_HSCALE@
VERA_DC_HSCALE_ADDR $ff #0 bitfield! VERA_DC_HSCALE!

\ DC_VSCALE - Display composer vertical scale register.
VERA_BASE_ADDR $24 + constant VERA_DC_VSCALE_ADDR

\ -- Address Mask Offset
VERA_DC_VSCALE_ADDR $ff #0 bitfield@ VERA_DC_VSCALE@
VERA_DC_VSCALE_ADDR $ff #0 bitfield! VERA_DC_VSCALE!

\ DC_HSTART - Display composer horizontal start register.
VERA_BASE_ADDR $28 + constant VERA_DC_HSTART_ADDR

\ -- Address Mask Offset
VERA_DC_HSTART_ADDR $3ff #0 bitfield@ VERA_DC_HSTART@
VERA_DC_HSTART_ADDR $3ff #0 bitfield! VERA_DC_HSTART!

\ DC_HSTOP - Display compser horizontal stop register.
VERA_BASE_ADDR $2c + constant VERA_DC_HSTOP_ADDR

\ -- Address Mask Offset
VERA_DC_HSTOP_ADDR $3ff #0 bitfield@ VERA_DC_HSTOP@
VERA_DC_HSTOP_ADDR $3ff #0 bitfield! VERA_DC_HSTOP!

\ DC_VSTART - Display composer vertical start register.
VERA_BASE_ADDR $30 + constant VERA_DC_VSTART_ADDR

\ -- Address Mask Offset
VERA_DC_VSTART_ADDR $3ff #0 bitfield@ VERA_DC_VSTART@
VERA_DC_VSTART_ADDR $3ff #0 bitfield! VERA_DC_VSTART!

\ DC_VSTOP - Display composer vertical stop register.
VERA_BASE_ADDR $34 + constant VERA_DC_VSTOP_ADDR

\ -- Address Mask Offset
VERA_DC_VSTOP_ADDR $3ff #0 bitfield@ VERA_DC_VSTOP@
VERA_DC_VSTOP_ADDR $3ff #0 bitfield! VERA_DC_VSTOP!

\ L0_CONFIG - Layer 0 Configuration regiser.
VERA_BASE_ADDR $40 + constant VERA_L0_CONFIG_ADDR

\ -- Address Mask Offset
VERA_L0_CONFIG_ADDR $3 #0 bitfield@ VERA_L0_CONFIG_COLORDEPTH@
VERA_L0_CONFIG_ADDR $3 #0 bitfield! VERA_L0_CONFIG_COLORDEPTH!

  #0 constant VERA_L0_CONFIG_COLORDEPTH_ONE_BPP \ 1 bpp. 
  #1 constant VERA_L0_CONFIG_COLORDEPTH_TWO_BPP \ 2 bpp. 
  #2 constant VERA_L0_CONFIG_COLORDEPTH_FOUR_BPP \ 4 bpp. 
  #3 constant VERA_L0_CONFIG_COLORDEPTH_EIGHT_BPP \ 8 bpp. 

VERA_L0_CONFIG_ADDR $4 #2 bitfield@ VERA_L0_CONFIG_BITMAPMODE@
VERA_L0_CONFIG_ADDR $4 #2 bitfield! VERA_L0_CONFIG_BITMAPMODE!
VERA_L0_CONFIG_ADDR $8 #3 bitfield@ VERA_L0_CONFIG_T256C@
VERA_L0_CONFIG_ADDR $8 #3 bitfield! VERA_L0_CONFIG_T256C!
VERA_L0_CONFIG_ADDR $30 #4 bitfield@ VERA_L0_CONFIG_MAP_WIDTH@
VERA_L0_CONFIG_ADDR $30 #4 bitfield! VERA_L0_CONFIG_MAP_WIDTH!

  #0 constant VERA_L0_CONFIG_MAP_WIDTH_W32 \ 32 tiles wide. 
  #1 constant VERA_L0_CONFIG_MAP_WIDTH_W64 \ 64 tiles wide. 
  #2 constant VERA_L0_CONFIG_MAP_WIDTH_W128 \ 128 tiles wide. 
  #3 constant VERA_L0_CONFIG_MAP_WIDTH_W256 \ 256 tiles wide. 

VERA_L0_CONFIG_ADDR $c0 #6 bitfield@ VERA_L0_CONFIG_MAP_HEIGHT@
VERA_L0_CONFIG_ADDR $c0 #6 bitfield! VERA_L0_CONFIG_MAP_HEIGHT!

  #0 constant VERA_L0_CONFIG_MAP_HEIGHT_H32 \ 32 tiles high. 
  #1 constant VERA_L0_CONFIG_MAP_HEIGHT_H64 \ 64 tiles high. 
  #2 constant VERA_L0_CONFIG_MAP_HEIGHT_H128 \ 128 tiles high. 
  #3 constant VERA_L0_CONFIG_MAP_HEIGHT_H256 \ 256 tiles high. 


\ L0_MAPBASE - Layer 0 map base register.
VERA_BASE_ADDR $44 + constant VERA_L0_MAPBASE_ADDR

\ -- Address Mask Offset
VERA_L0_MAPBASE_ADDR $ff #0 bitfield@ VERA_L0_MAPBASE@
VERA_L0_MAPBASE_ADDR $ff #0 bitfield! VERA_L0_MAPBASE!

\ L0_TILEBASE - Layer 0 tile base register.
VERA_BASE_ADDR $48 + constant VERA_L0_TILEBASE_ADDR

\ -- Address Mask Offset
VERA_L0_TILEBASE_ADDR $1 #0 bitfield@ VERA_L0_TILEBASE_TILE_BITMAP_WIDTH@
VERA_L0_TILEBASE_ADDR $1 #0 bitfield! VERA_L0_TILEBASE_TILE_BITMAP_WIDTH!

  #0 constant VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_W_8_320 \ 8 pixel tile width, 320 pixels bitmap width. 
  #1 constant VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_W_16_640 \ 16 pixel tile width, 640 pixels bitmap width. 

VERA_L0_TILEBASE_ADDR $2 #1 bitfield@ VERA_L0_TILEBASE_TILE_HEIGHT@
VERA_L0_TILEBASE_ADDR $2 #1 bitfield! VERA_L0_TILEBASE_TILE_HEIGHT!

  #0 constant VERA_L0_TILEBASE_TILE_HEIGHT_H8 \ 8 pixel tile height. 
  #1 constant VERA_L0_TILEBASE_TILE_HEIGHT_H16 \ 16 pixel tile height. 

VERA_L0_TILEBASE_ADDR $fc #2 bitfield@ VERA_L0_TILEBASE_TILE_BASEADDR@
VERA_L0_TILEBASE_ADDR $fc #2 bitfield! VERA_L0_TILEBASE_TILE_BASEADDR!

\ L0_HSCROLL - Layer 0 horizontal scroll register.
VERA_BASE_ADDR $50 + constant VERA_L0_HSCROLL_ADDR

\ -- Address Mask Offset
VERA_L0_HSCROLL_ADDR $ff #0 bitfield@ VERA_L0_HSCROLL_HSCROLL_7_0@
VERA_L0_HSCROLL_ADDR $ff #0 bitfield! VERA_L0_HSCROLL_HSCROLL_7_0!
VERA_L0_HSCROLL_ADDR $f00 #8 bitfield@ VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET@
VERA_L0_HSCROLL_ADDR $f00 #8 bitfield! VERA_L0_HSCROLL_HSCROLL_11_8_PALOFFSET!

\ L0_VSCROLL - Layer 0 vertical scroll register.
VERA_BASE_ADDR $54 + constant VERA_L0_VSCROLL_ADDR

\ -- Address Mask Offset
VERA_L0_VSCROLL_ADDR $fff #0 bitfield@ VERA_L0_VSCROLL@
VERA_L0_VSCROLL_ADDR $fff #0 bitfield! VERA_L0_VSCROLL!

\ L1_CONFIG - Layer 1 Configuration regiser.
VERA_BASE_ADDR $80 + constant VERA_L1_CONFIG_ADDR

\ -- Address Mask Offset
VERA_L1_CONFIG_ADDR $3 #0 bitfield@ VERA_L1_CONFIG_COLORDEPTH@
VERA_L1_CONFIG_ADDR $3 #0 bitfield! VERA_L1_CONFIG_COLORDEPTH!

  #0 constant VERA_L1_CONFIG_COLORDEPTH_ONE_BPP \ 1 bpp. 
  #1 constant VERA_L1_CONFIG_COLORDEPTH_TWO_BPP \ 2 bpp. 
  #2 constant VERA_L1_CONFIG_COLORDEPTH_FOUR_BPP \ 4 bpp. 
  #3 constant VERA_L1_CONFIG_COLORDEPTH_EIGHT_BPP \ 8 bpp. 

VERA_L1_CONFIG_ADDR $4 #2 bitfield@ VERA_L1_CONFIG_BITMAPMODE@
VERA_L1_CONFIG_ADDR $4 #2 bitfield! VERA_L1_CONFIG_BITMAPMODE!
VERA_L1_CONFIG_ADDR $8 #3 bitfield@ VERA_L1_CONFIG_T256C@
VERA_L1_CONFIG_ADDR $8 #3 bitfield! VERA_L1_CONFIG_T256C!
VERA_L1_CONFIG_ADDR $30 #4 bitfield@ VERA_L1_CONFIG_MAP_WIDTH@
VERA_L1_CONFIG_ADDR $30 #4 bitfield! VERA_L1_CONFIG_MAP_WIDTH!

  #0 constant VERA_L1_CONFIG_MAP_WIDTH_W32 \ 32 tiles wide. 
  #1 constant VERA_L1_CONFIG_MAP_WIDTH_W64 \ 64 tiles wide. 
  #2 constant VERA_L1_CONFIG_MAP_WIDTH_W128 \ 128 tiles wide. 
  #3 constant VERA_L1_CONFIG_MAP_WIDTH_W256 \ 256 tiles wide. 

VERA_L1_CONFIG_ADDR $c0 #6 bitfield@ VERA_L1_CONFIG_MAP_HEIGHT@
VERA_L1_CONFIG_ADDR $c0 #6 bitfield! VERA_L1_CONFIG_MAP_HEIGHT!

  #0 constant VERA_L1_CONFIG_MAP_HEIGHT_H32 \ 32 tiles high. 
  #1 constant VERA_L1_CONFIG_MAP_HEIGHT_H64 \ 64 tiles high. 
  #2 constant VERA_L1_CONFIG_MAP_HEIGHT_H128 \ 128 tiles high. 
  #3 constant VERA_L1_CONFIG_MAP_HEIGHT_H256 \ 256 tiles high. 


\ L1_MAPBASE - Layer 1 map base register.
VERA_BASE_ADDR $84 + constant VERA_L1_MAPBASE_ADDR

\ -- Address Mask Offset
VERA_L1_MAPBASE_ADDR $ff #0 bitfield@ VERA_L1_MAPBASE@
VERA_L1_MAPBASE_ADDR $ff #0 bitfield! VERA_L1_MAPBASE!

\ L1_TILEBASE - Layer 1 tile base register.
VERA_BASE_ADDR $88 + constant VERA_L1_TILEBASE_ADDR

\ -- Address Mask Offset
VERA_L1_TILEBASE_ADDR $1 #0 bitfield@ VERA_L1_TILEBASE_TILE_BITMAP_WIDTH@
VERA_L1_TILEBASE_ADDR $1 #0 bitfield! VERA_L1_TILEBASE_TILE_BITMAP_WIDTH!

  #0 constant VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_W_8_320 \ 8 pixel tile width, 320 pixels bitmap width. 
  #1 constant VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_W_16_640 \ 16 pixel tile width, 640 pixels bitmap width. 

VERA_L1_TILEBASE_ADDR $2 #1 bitfield@ VERA_L1_TILEBASE_TILE_HEIGHT@
VERA_L1_TILEBASE_ADDR $2 #1 bitfield! VERA_L1_TILEBASE_TILE_HEIGHT!

  #0 constant VERA_L1_TILEBASE_TILE_HEIGHT_H8 \ 8 pixel tile height. 
  #1 constant VERA_L1_TILEBASE_TILE_HEIGHT_H16 \ 16 pixel tile height. 

VERA_L1_TILEBASE_ADDR $fc #2 bitfield@ VERA_L1_TILEBASE_TILE_BASEADDR@
VERA_L1_TILEBASE_ADDR $fc #2 bitfield! VERA_L1_TILEBASE_TILE_BASEADDR!

\ L1_HSCROLL - Layer 1 horizontal scroll register.
VERA_BASE_ADDR $90 + constant VERA_L1_HSCROLL_ADDR

\ -- Address Mask Offset
VERA_L1_HSCROLL_ADDR $ff #0 bitfield@ VERA_L1_HSCROLL_HSCROLL_7_0@
VERA_L1_HSCROLL_ADDR $ff #0 bitfield! VERA_L1_HSCROLL_HSCROLL_7_0!
VERA_L1_HSCROLL_ADDR $f00 #8 bitfield@ VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET@
VERA_L1_HSCROLL_ADDR $f00 #8 bitfield! VERA_L1_HSCROLL_HSCROLL_11_8_PALOFFSET!

\ L1_VSCROLL - Layer 1 vertical scroll register.
VERA_BASE_ADDR $94 + constant VERA_L1_VSCROLL_ADDR

\ -- Address Mask Offset
VERA_L1_VSCROLL_ADDR $fff #0 bitfield@ VERA_L1_VSCROLL@
VERA_L1_VSCROLL_ADDR $fff #0 bitfield! VERA_L1_VSCROLL!
