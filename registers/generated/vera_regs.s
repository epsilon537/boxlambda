# --- VERA

.equ VERA_BASE_ADDR, 0x12000000

# CTRL_STATUS - Control/Status register.
.equ VERA_CTRL_STATUS_ADDR, 0x0

# CTRL_STATUS.SBNK - Active sprite bank.
.equ VERA_CTRL_STATUS_SBNK_WIDTH, 1
.equ VERA_CTRL_STATUS_SBNK_LSB, 0
.equ VERA_CTRL_STATUS_SBNK_MASK, 0x1
# CTRL_STATUS.CAPTURE_EN - Enable VGA line capture. Bit returns to 0 when capture has completed.
.equ VERA_CTRL_STATUS_CAPTURE_EN_WIDTH, 1
.equ VERA_CTRL_STATUS_CAPTURE_EN_LSB, 1
.equ VERA_CTRL_STATUS_CAPTURE_EN_MASK, 0x2
# DC_BORDER - Display composer border register.
.equ VERA_DC_BORDER_ADDR, 0x4

# DC_BORDER.BORDER_COLOR - Border color
.equ VERA_DC_BORDER_BORDER_COLOR_WIDTH, 8
.equ VERA_DC_BORDER_BORDER_COLOR_LSB, 0
.equ VERA_DC_BORDER_BORDER_COLOR_MASK, 0xff
# IEN - Interrupt enable register.
.equ VERA_IEN_ADDR, 0x8

# IEN.VSYNC - Vertical sync interrupt enable.
.equ VERA_IEN_VSYNC_WIDTH, 1
.equ VERA_IEN_VSYNC_LSB, 0
.equ VERA_IEN_VSYNC_MASK, 0x1
# IEN.LINE - Line interrupt enable.
.equ VERA_IEN_LINE_WIDTH, 1
.equ VERA_IEN_LINE_LSB, 1
.equ VERA_IEN_LINE_MASK, 0x2
# IEN.SPRCOL - Sprite collision interrupt enable.
.equ VERA_IEN_SPRCOL_WIDTH, 1
.equ VERA_IEN_SPRCOL_LSB, 2
.equ VERA_IEN_SPRCOL_MASK, 0x4
# ISR - Interrupt status register.
.equ VERA_ISR_ADDR, 0xc

# ISR.VSYNC - Vertical sync interrupt status.
.equ VERA_ISR_VSYNC_WIDTH, 1
.equ VERA_ISR_VSYNC_LSB, 0
.equ VERA_ISR_VSYNC_MASK, 0x1
# ISR.LINE - Line interrupt status.
.equ VERA_ISR_LINE_WIDTH, 1
.equ VERA_ISR_LINE_LSB, 1
.equ VERA_ISR_LINE_MASK, 0x2
# ISR.SPRCOL - Sprite collision interrupt status.
.equ VERA_ISR_SPRCOL_WIDTH, 1
.equ VERA_ISR_SPRCOL_LSB, 2
.equ VERA_ISR_SPRCOL_MASK, 0x4
# ISR.SPR_COLLISIONS - Sprite collisions as determined by sprite renderer.
.equ VERA_ISR_SPR_COLLISIONS_WIDTH, 4
.equ VERA_ISR_SPR_COLLISIONS_LSB, 4
.equ VERA_ISR_SPR_COLLISIONS_MASK, 0xf0
# IRQLINE - Interrupt line register.
.equ VERA_IRQLINE_ADDR, 0x10

# IRQLINE.VALUE - Scanline on which to generate line interrupt.
.equ VERA_IRQLINE_VALUE_WIDTH, 10
.equ VERA_IRQLINE_VALUE_LSB, 0
.equ VERA_IRQLINE_VALUE_MASK, 0x3ff
# SCANLINE - Scanline register
.equ VERA_SCANLINE_ADDR, 0x14

# SCANLINE.VALUE - Current scanline.
.equ VERA_SCANLINE_VALUE_WIDTH, 10
.equ VERA_SCANLINE_VALUE_LSB, 0
.equ VERA_SCANLINE_VALUE_MASK, 0x3ff
# DC_VIDEO - Display composer video register.
.equ VERA_DC_VIDEO_ADDR, 0x18

# DC_VIDEO.OUTPUT_MODE - Video output mode.
.equ VERA_DC_VIDEO_OUTPUT_MODE_WIDTH, 2
.equ VERA_DC_VIDEO_OUTPUT_MODE_LSB, 0
.equ VERA_DC_VIDEO_OUTPUT_MODE_MASK, 0x3
# DC_VIDEO.L0_ENABLE - Enable Layer 0.
.equ VERA_DC_VIDEO_L0_ENABLE_WIDTH, 1
.equ VERA_DC_VIDEO_L0_ENABLE_LSB, 4
.equ VERA_DC_VIDEO_L0_ENABLE_MASK, 0x10
# DC_VIDEO.L1_ENABLE - Enable layer 1.
.equ VERA_DC_VIDEO_L1_ENABLE_WIDTH, 1
.equ VERA_DC_VIDEO_L1_ENABLE_LSB, 5
.equ VERA_DC_VIDEO_L1_ENABLE_MASK, 0x20
# DC_VIDEO.SPR_ENABLE - Enable sprites.
.equ VERA_DC_VIDEO_SPR_ENABLE_WIDTH, 1
.equ VERA_DC_VIDEO_SPR_ENABLE_LSB, 6
.equ VERA_DC_VIDEO_SPR_ENABLE_MASK, 0x40
# DC_HSCALE - Display composer horizontal scale register.
.equ VERA_DC_HSCALE_ADDR, 0x20

# DC_HSCALE.VALUE - the horizonal fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
.equ VERA_DC_HSCALE_VALUE_WIDTH, 8
.equ VERA_DC_HSCALE_VALUE_LSB, 0
.equ VERA_DC_HSCALE_VALUE_MASK, 0xff
# DC_VSCALE - Display composer vertical scale register.
.equ VERA_DC_VSCALE_ADDR, 0x24

# DC_VSCALE.VALUE - the vertical fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
.equ VERA_DC_VSCALE_VALUE_WIDTH, 8
.equ VERA_DC_VSCALE_VALUE_LSB, 0
.equ VERA_DC_VSCALE_VALUE_MASK, 0xff
# DC_HSTART - Display composer horizontal start register.
.equ VERA_DC_HSTART_ADDR, 0x28

# DC_HSTART.VALUE - Horizontal start of active part of screen in 640x480 space.
.equ VERA_DC_HSTART_VALUE_WIDTH, 10
.equ VERA_DC_HSTART_VALUE_LSB, 0
.equ VERA_DC_HSTART_VALUE_MASK, 0x3ff
# DC_HSTOP - Display compser horizontal stop register.
.equ VERA_DC_HSTOP_ADDR, 0x2c

# DC_HSTOP.VALUE - Horizontal stop of active part of screen in 640x480 space.
.equ VERA_DC_HSTOP_VALUE_WIDTH, 10
.equ VERA_DC_HSTOP_VALUE_LSB, 0
.equ VERA_DC_HSTOP_VALUE_MASK, 0x3ff
# DC_VSTART - Display composer vertical start register.
.equ VERA_DC_VSTART_ADDR, 0x30

# DC_VSTART.VALUE - Vertical start of active part of screen in 640x480 space.
.equ VERA_DC_VSTART_VALUE_WIDTH, 10
.equ VERA_DC_VSTART_VALUE_LSB, 0
.equ VERA_DC_VSTART_VALUE_MASK, 0x3ff
# DC_VSTOP - Display composer vertical stop register.
.equ VERA_DC_VSTOP_ADDR, 0x34

# DC_VSTOP.VALUE - Vertical stop of active part of screen in 640x480 space.
.equ VERA_DC_VSTOP_VALUE_WIDTH, 10
.equ VERA_DC_VSTOP_VALUE_LSB, 0
.equ VERA_DC_VSTOP_VALUE_MASK, 0x3ff
# L0_CONFIG - Layer 0 Configuration regiser.
.equ VERA_L0_CONFIG_ADDR, 0x40

# L0_CONFIG.COLOR_DEPTH - Number of bits pers pixel to encode color information.
.equ VERA_L0_CONFIG_COLOR_DEPTH_WIDTH, 2
.equ VERA_L0_CONFIG_COLOR_DEPTH_LSB, 0
.equ VERA_L0_CONFIG_COLOR_DEPTH_MASK, 0x3
# L0_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
.equ VERA_L0_CONFIG_BITMAP_MODE_WIDTH, 1
.equ VERA_L0_CONFIG_BITMAP_MODE_LSB, 2
.equ VERA_L0_CONFIG_BITMAP_MODE_MASK, 0x4
# L0_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
.equ VERA_L0_CONFIG_T256C_WIDTH, 1
.equ VERA_L0_CONFIG_T256C_LSB, 3
.equ VERA_L0_CONFIG_T256C_MASK, 0x8
# L0_CONFIG.MAP_WIDTH - Tile map width.
.equ VERA_L0_CONFIG_MAP_WIDTH_WIDTH, 2
.equ VERA_L0_CONFIG_MAP_WIDTH_LSB, 4
.equ VERA_L0_CONFIG_MAP_WIDTH_MASK, 0x30
# L0_CONFIG.MAP_HEIGHT - Tile map height.
.equ VERA_L0_CONFIG_MAP_HEIGHT_WIDTH, 2
.equ VERA_L0_CONFIG_MAP_HEIGHT_LSB, 6
.equ VERA_L0_CONFIG_MAP_HEIGHT_MASK, 0xc0
# L0_MAPBASE - Layer 0 map base register.
.equ VERA_L0_MAPBASE_ADDR, 0x44

# L0_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
.equ VERA_L0_MAPBASE_ADDR_16_9_WIDTH, 8
.equ VERA_L0_MAPBASE_ADDR_16_9_LSB, 0
.equ VERA_L0_MAPBASE_ADDR_16_9_MASK, 0xff
# L0_TILEBASE - Layer 0 tile base register.
.equ VERA_L0_TILEBASE_ADDR, 0x48

# L0_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
.equ VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_WIDTH, 1
.equ VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_LSB, 0
.equ VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_MASK, 0x1
# L0_TILEBASE.TILE_HEIGHT - Tile height.
.equ VERA_L0_TILEBASE_TILE_HEIGHT_WIDTH, 1
.equ VERA_L0_TILEBASE_TILE_HEIGHT_LSB, 1
.equ VERA_L0_TILEBASE_TILE_HEIGHT_MASK, 0x2
# L0_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
.equ VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH, 6
.equ VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_LSB, 2
.equ VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_MASK, 0xfc
# L0_HSCROLL - Layer 0 horizontal scroll register.
.equ VERA_L0_HSCROLL_ADDR, 0x50

# L0_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
.equ VERA_L0_HSCROLL_HSCROLL_7_0_WIDTH, 8
.equ VERA_L0_HSCROLL_HSCROLL_7_0_LSB, 0
.equ VERA_L0_HSCROLL_HSCROLL_7_0_MASK, 0xff
# L0_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
.equ VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH, 4
.equ VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB, 8
.equ VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK, 0xf00
# L0_VSCROLL - Layer 0 vertical scroll register.
.equ VERA_L0_VSCROLL_ADDR, 0x54

# L0_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
.equ VERA_L0_VSCROLL_VALUE_WIDTH, 12
.equ VERA_L0_VSCROLL_VALUE_LSB, 0
.equ VERA_L0_VSCROLL_VALUE_MASK, 0xfff
# L1_CONFIG - Layer 1 Configuration regiser.
.equ VERA_L1_CONFIG_ADDR, 0x80

# L1_CONFIG.COLOR_DEPTH - Number of bits pers pixel to encode color information.
.equ VERA_L1_CONFIG_COLOR_DEPTH_WIDTH, 2
.equ VERA_L1_CONFIG_COLOR_DEPTH_LSB, 0
.equ VERA_L1_CONFIG_COLOR_DEPTH_MASK, 0x3
# L1_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
.equ VERA_L1_CONFIG_BITMAP_MODE_WIDTH, 1
.equ VERA_L1_CONFIG_BITMAP_MODE_LSB, 2
.equ VERA_L1_CONFIG_BITMAP_MODE_MASK, 0x4
# L1_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
.equ VERA_L1_CONFIG_T256C_WIDTH, 1
.equ VERA_L1_CONFIG_T256C_LSB, 3
.equ VERA_L1_CONFIG_T256C_MASK, 0x8
# L1_CONFIG.MAP_WIDTH - Tile map width.
.equ VERA_L1_CONFIG_MAP_WIDTH_WIDTH, 2
.equ VERA_L1_CONFIG_MAP_WIDTH_LSB, 4
.equ VERA_L1_CONFIG_MAP_WIDTH_MASK, 0x30
# L1_CONFIG.MAP_HEIGHT - Tile map height.
.equ VERA_L1_CONFIG_MAP_HEIGHT_WIDTH, 2
.equ VERA_L1_CONFIG_MAP_HEIGHT_LSB, 6
.equ VERA_L1_CONFIG_MAP_HEIGHT_MASK, 0xc0
# L1_MAPBASE - Layer 1 map base register.
.equ VERA_L1_MAPBASE_ADDR, 0x84

# L1_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
.equ VERA_L1_MAPBASE_ADDR_16_9_WIDTH, 8
.equ VERA_L1_MAPBASE_ADDR_16_9_LSB, 0
.equ VERA_L1_MAPBASE_ADDR_16_9_MASK, 0xff
# L1_TILEBASE - Layer 1 tile base register.
.equ VERA_L1_TILEBASE_ADDR, 0x88

# L1_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
.equ VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_WIDTH, 1
.equ VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_LSB, 0
.equ VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_MASK, 0x1
# L1_TILEBASE.TILE_HEIGHT - Tile height.
.equ VERA_L1_TILEBASE_TILE_HEIGHT_WIDTH, 1
.equ VERA_L1_TILEBASE_TILE_HEIGHT_LSB, 1
.equ VERA_L1_TILEBASE_TILE_HEIGHT_MASK, 0x2
# L1_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
.equ VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH, 6
.equ VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_LSB, 2
.equ VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_MASK, 0xfc
# L1_HSCROLL - Layer 1 horizontal scroll register.
.equ VERA_L1_HSCROLL_ADDR, 0x90

# L1_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
.equ VERA_L1_HSCROLL_HSCROLL_7_0_WIDTH, 8
.equ VERA_L1_HSCROLL_HSCROLL_7_0_LSB, 0
.equ VERA_L1_HSCROLL_HSCROLL_7_0_MASK, 0xff
# L1_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
.equ VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH, 4
.equ VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB, 8
.equ VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK, 0xf00
# L1_VSCROLL - Layer 1 vertical scroll register.
.equ VERA_L1_VSCROLL_ADDR, 0x94

# L1_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
.equ VERA_L1_VSCROLL_VALUE_WIDTH, 12
.equ VERA_L1_VSCROLL_VALUE_LSB, 0
.equ VERA_L1_VSCROLL_VALUE_MASK, 0xfff
