\ --- VERA

$12000000 constant VERA_BASE_ADDR

\ CTRL_STATUS - Control/Status register.
$0 constant VERA_CTRL_STATUS_ADDR

\ CTRL_STATUS.SBNK - Active sprite bank.
1 constant VERA_CTRL_STATUS_SBNK_WIDTH
0 constant VERA_CTRL_STATUS_SBNK_LSB
$1 constant VERA_CTRL_STATUS_SBNK_MASK
\ CTRL_STATUS.CAPTURE_EN - Enable VGA line capture. Bit returns to 0 when capture has completed.
1 constant VERA_CTRL_STATUS_CAPTURE_EN_WIDTH
1 constant VERA_CTRL_STATUS_CAPTURE_EN_LSB
$2 constant VERA_CTRL_STATUS_CAPTURE_EN_MASK
\ DC_BORDER - Display composer border register.
$4 constant VERA_DC_BORDER_ADDR

\ DC_BORDER.BORDER_COLOR - Border color
8 constant VERA_DC_BORDER_BORDER_COLOR_WIDTH
0 constant VERA_DC_BORDER_BORDER_COLOR_LSB
$ff constant VERA_DC_BORDER_BORDER_COLOR_MASK
\ IEN - Interrupt enable register.
$8 constant VERA_IEN_ADDR

\ IEN.VSYNC - Vertical sync interrupt enable.
1 constant VERA_IEN_VSYNC_WIDTH
0 constant VERA_IEN_VSYNC_LSB
$1 constant VERA_IEN_VSYNC_MASK
\ IEN.LINE - Line interrupt enable.
1 constant VERA_IEN_LINE_WIDTH
1 constant VERA_IEN_LINE_LSB
$2 constant VERA_IEN_LINE_MASK
\ IEN.SPRCOL - Sprite collision interrupt enable.
1 constant VERA_IEN_SPRCOL_WIDTH
2 constant VERA_IEN_SPRCOL_LSB
$4 constant VERA_IEN_SPRCOL_MASK
\ ISR - Interrupt status register.
$c constant VERA_ISR_ADDR

\ ISR.VSYNC - Vertical sync interrupt status.
1 constant VERA_ISR_VSYNC_WIDTH
0 constant VERA_ISR_VSYNC_LSB
$1 constant VERA_ISR_VSYNC_MASK
\ ISR.LINE - Line interrupt status.
1 constant VERA_ISR_LINE_WIDTH
1 constant VERA_ISR_LINE_LSB
$2 constant VERA_ISR_LINE_MASK
\ ISR.SPRCOL - Sprite collision interrupt status.
1 constant VERA_ISR_SPRCOL_WIDTH
2 constant VERA_ISR_SPRCOL_LSB
$4 constant VERA_ISR_SPRCOL_MASK
\ ISR.SPR_COLLISIONS - Sprite collisions as determined by sprite renderer.
4 constant VERA_ISR_SPR_COLLISIONS_WIDTH
4 constant VERA_ISR_SPR_COLLISIONS_LSB
$f0 constant VERA_ISR_SPR_COLLISIONS_MASK
\ IRQLINE - Interrupt line register.
$10 constant VERA_IRQLINE_ADDR

\ IRQLINE.VALUE - Scanline on which to generate line interrupt.
10 constant VERA_IRQLINE_VALUE_WIDTH
0 constant VERA_IRQLINE_VALUE_LSB
$3ff constant VERA_IRQLINE_VALUE_MASK
\ SCANLINE - Scanline register
$14 constant VERA_SCANLINE_ADDR

\ SCANLINE.VALUE - Current scanline.
10 constant VERA_SCANLINE_VALUE_WIDTH
0 constant VERA_SCANLINE_VALUE_LSB
$3ff constant VERA_SCANLINE_VALUE_MASK
\ DC_VIDEO - Display composer video register.
$18 constant VERA_DC_VIDEO_ADDR

\ DC_VIDEO.OUTPUT_MODE - Video output mode.
2 constant VERA_DC_VIDEO_OUTPUT_MODE_WIDTH
0 constant VERA_DC_VIDEO_OUTPUT_MODE_LSB
$3 constant VERA_DC_VIDEO_OUTPUT_MODE_MASK
\ DC_VIDEO.L0_ENABLE - Enable Layer 0.
1 constant VERA_DC_VIDEO_L0_ENABLE_WIDTH
4 constant VERA_DC_VIDEO_L0_ENABLE_LSB
$10 constant VERA_DC_VIDEO_L0_ENABLE_MASK
\ DC_VIDEO.L1_ENABLE - Enable layer 1.
1 constant VERA_DC_VIDEO_L1_ENABLE_WIDTH
5 constant VERA_DC_VIDEO_L1_ENABLE_LSB
$20 constant VERA_DC_VIDEO_L1_ENABLE_MASK
\ DC_VIDEO.SPR_ENABLE - Enable sprites.
1 constant VERA_DC_VIDEO_SPR_ENABLE_WIDTH
6 constant VERA_DC_VIDEO_SPR_ENABLE_LSB
$40 constant VERA_DC_VIDEO_SPR_ENABLE_MASK
\ DC_HSCALE - Display composer horizontal scale register.
$20 constant VERA_DC_HSCALE_ADDR

\ DC_HSCALE.VALUE - the horizontal fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
8 constant VERA_DC_HSCALE_VALUE_WIDTH
0 constant VERA_DC_HSCALE_VALUE_LSB
$ff constant VERA_DC_HSCALE_VALUE_MASK
\ DC_VSCALE - Display composer vertical scale register.
$24 constant VERA_DC_VSCALE_ADDR

\ DC_VSCALE.VALUE - the vertical fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
8 constant VERA_DC_VSCALE_VALUE_WIDTH
0 constant VERA_DC_VSCALE_VALUE_LSB
$ff constant VERA_DC_VSCALE_VALUE_MASK
\ DC_HSTART - Display composer horizontal start register.
$28 constant VERA_DC_HSTART_ADDR

\ DC_HSTART.VALUE - Horizontal start of active part of screen in 640x480 space.
10 constant VERA_DC_HSTART_VALUE_WIDTH
0 constant VERA_DC_HSTART_VALUE_LSB
$3ff constant VERA_DC_HSTART_VALUE_MASK
\ DC_HSTOP - Display compser horizontal stop register.
$2c constant VERA_DC_HSTOP_ADDR

\ DC_HSTOP.VALUE - Horizontal stop of active part of screen in 640x480 space.
10 constant VERA_DC_HSTOP_VALUE_WIDTH
0 constant VERA_DC_HSTOP_VALUE_LSB
$3ff constant VERA_DC_HSTOP_VALUE_MASK
\ DC_VSTART - Display composer vertical start register.
$30 constant VERA_DC_VSTART_ADDR

\ DC_VSTART.VALUE - Vertical start of active part of screen in 640x480 space.
10 constant VERA_DC_VSTART_VALUE_WIDTH
0 constant VERA_DC_VSTART_VALUE_LSB
$3ff constant VERA_DC_VSTART_VALUE_MASK
\ DC_VSTOP - Display composer vertical stop register.
$34 constant VERA_DC_VSTOP_ADDR

\ DC_VSTOP.VALUE - Vertical stop of active part of screen in 640x480 space.
10 constant VERA_DC_VSTOP_VALUE_WIDTH
0 constant VERA_DC_VSTOP_VALUE_LSB
$3ff constant VERA_DC_VSTOP_VALUE_MASK
\ L0_CONFIG - Layer 0 Configuration regiser.
$40 constant VERA_L0_CONFIG_ADDR

\ L0_CONFIG.COLOR_DEPTH - Number of bits per pixel to encode color information.
2 constant VERA_L0_CONFIG_COLOR_DEPTH_WIDTH
0 constant VERA_L0_CONFIG_COLOR_DEPTH_LSB
$3 constant VERA_L0_CONFIG_COLOR_DEPTH_MASK
\ L0_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
1 constant VERA_L0_CONFIG_BITMAP_MODE_WIDTH
2 constant VERA_L0_CONFIG_BITMAP_MODE_LSB
$4 constant VERA_L0_CONFIG_BITMAP_MODE_MASK
\ L0_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
1 constant VERA_L0_CONFIG_T256C_WIDTH
3 constant VERA_L0_CONFIG_T256C_LSB
$8 constant VERA_L0_CONFIG_T256C_MASK
\ L0_CONFIG.MAP_WIDTH - Tile map width.
2 constant VERA_L0_CONFIG_MAP_WIDTH_WIDTH
4 constant VERA_L0_CONFIG_MAP_WIDTH_LSB
$30 constant VERA_L0_CONFIG_MAP_WIDTH_MASK
\ L0_CONFIG.MAP_HEIGHT - Tile map height.
2 constant VERA_L0_CONFIG_MAP_HEIGHT_WIDTH
6 constant VERA_L0_CONFIG_MAP_HEIGHT_LSB
$c0 constant VERA_L0_CONFIG_MAP_HEIGHT_MASK
\ L0_MAPBASE - Layer 0 map base register.
$44 constant VERA_L0_MAPBASE_ADDR

\ L0_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
8 constant VERA_L0_MAPBASE_ADDR_16_9_WIDTH
0 constant VERA_L0_MAPBASE_ADDR_16_9_LSB
$ff constant VERA_L0_MAPBASE_ADDR_16_9_MASK
\ L0_TILEBASE - Layer 0 tile base register.
$48 constant VERA_L0_TILEBASE_ADDR

\ L0_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
1 constant VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_WIDTH
0 constant VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_LSB
$1 constant VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_MASK
\ L0_TILEBASE.TILE_HEIGHT - Tile height.
1 constant VERA_L0_TILEBASE_TILE_HEIGHT_WIDTH
1 constant VERA_L0_TILEBASE_TILE_HEIGHT_LSB
$2 constant VERA_L0_TILEBASE_TILE_HEIGHT_MASK
\ L0_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
6 constant VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH
2 constant VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_LSB
$fc constant VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_MASK
\ L0_HSCROLL - Layer 0 horizontal scroll register.
$50 constant VERA_L0_HSCROLL_ADDR

\ L0_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
8 constant VERA_L0_HSCROLL_HSCROLL_7_0_WIDTH
0 constant VERA_L0_HSCROLL_HSCROLL_7_0_LSB
$ff constant VERA_L0_HSCROLL_HSCROLL_7_0_MASK
\ L0_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
4 constant VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH
8 constant VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB
$f00 constant VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK
\ L0_VSCROLL - Layer 0 vertical scroll register.
$54 constant VERA_L0_VSCROLL_ADDR

\ L0_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
12 constant VERA_L0_VSCROLL_VALUE_WIDTH
0 constant VERA_L0_VSCROLL_VALUE_LSB
$fff constant VERA_L0_VSCROLL_VALUE_MASK
\ L1_CONFIG - Layer 1 Configuration regiser.
$80 constant VERA_L1_CONFIG_ADDR

\ L1_CONFIG.COLOR_DEPTH - Number of bits per pixel to encode color information.
2 constant VERA_L1_CONFIG_COLOR_DEPTH_WIDTH
0 constant VERA_L1_CONFIG_COLOR_DEPTH_LSB
$3 constant VERA_L1_CONFIG_COLOR_DEPTH_MASK
\ L1_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
1 constant VERA_L1_CONFIG_BITMAP_MODE_WIDTH
2 constant VERA_L1_CONFIG_BITMAP_MODE_LSB
$4 constant VERA_L1_CONFIG_BITMAP_MODE_MASK
\ L1_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
1 constant VERA_L1_CONFIG_T256C_WIDTH
3 constant VERA_L1_CONFIG_T256C_LSB
$8 constant VERA_L1_CONFIG_T256C_MASK
\ L1_CONFIG.MAP_WIDTH - Tile map width.
2 constant VERA_L1_CONFIG_MAP_WIDTH_WIDTH
4 constant VERA_L1_CONFIG_MAP_WIDTH_LSB
$30 constant VERA_L1_CONFIG_MAP_WIDTH_MASK
\ L1_CONFIG.MAP_HEIGHT - Tile map height.
2 constant VERA_L1_CONFIG_MAP_HEIGHT_WIDTH
6 constant VERA_L1_CONFIG_MAP_HEIGHT_LSB
$c0 constant VERA_L1_CONFIG_MAP_HEIGHT_MASK
\ L1_MAPBASE - Layer 1 map base register.
$84 constant VERA_L1_MAPBASE_ADDR

\ L1_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
8 constant VERA_L1_MAPBASE_ADDR_16_9_WIDTH
0 constant VERA_L1_MAPBASE_ADDR_16_9_LSB
$ff constant VERA_L1_MAPBASE_ADDR_16_9_MASK
\ L1_TILEBASE - Layer 1 tile base register.
$88 constant VERA_L1_TILEBASE_ADDR

\ L1_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
1 constant VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_WIDTH
0 constant VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_LSB
$1 constant VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_MASK
\ L1_TILEBASE.TILE_HEIGHT - Tile height.
1 constant VERA_L1_TILEBASE_TILE_HEIGHT_WIDTH
1 constant VERA_L1_TILEBASE_TILE_HEIGHT_LSB
$2 constant VERA_L1_TILEBASE_TILE_HEIGHT_MASK
\ L1_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
6 constant VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH
2 constant VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_LSB
$fc constant VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_MASK
\ L1_HSCROLL - Layer 1 horizontal scroll register.
$90 constant VERA_L1_HSCROLL_ADDR

\ L1_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
8 constant VERA_L1_HSCROLL_HSCROLL_7_0_WIDTH
0 constant VERA_L1_HSCROLL_HSCROLL_7_0_LSB
$ff constant VERA_L1_HSCROLL_HSCROLL_7_0_MASK
\ L1_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
4 constant VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH
8 constant VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB
$f00 constant VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK
\ L1_VSCROLL - Layer 1 vertical scroll register.
$94 constant VERA_L1_VSCROLL_ADDR

\ L1_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
12 constant VERA_L1_VSCROLL_VALUE_WIDTH
0 constant VERA_L1_VSCROLL_VALUE_LSB
$fff constant VERA_L1_VSCROLL_VALUE_MASK
