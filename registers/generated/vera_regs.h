// Created with Corsair v1.0.4
#ifndef __VERA_REGS_H
#define __VERA_REGS_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define VERA_BASE_ADDR 0x12000000

// CTRL - Control register.
#define VERA_CTRL_ADDR 0x0
#define VERA_CTRL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t SBNK : 1; // Active sprite bank.
    uint32_t : 31; // reserved
  };
} vera_ctrl_t;

// CTRL.SBNK - Active sprite bank.
#define VERA_CTRL_SBNK_WIDTH 1
#define VERA_CTRL_SBNK_LSB 0
#define VERA_CTRL_SBNK_MASK 0x1
#define VERA_CTRL_SBNK_RESET 0x0

// DC_BORDER - Display composer border register.
#define VERA_DC_BORDER_ADDR 0x4
#define VERA_DC_BORDER_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t BORDER_COLOR : 8; // Border color
    uint32_t : 24; // reserved
  };
} vera_dc_border_t;

// DC_BORDER.BORDER_COLOR - Border color
#define VERA_DC_BORDER_BORDER_COLOR_WIDTH 8
#define VERA_DC_BORDER_BORDER_COLOR_LSB 0
#define VERA_DC_BORDER_BORDER_COLOR_MASK 0xff
#define VERA_DC_BORDER_BORDER_COLOR_RESET 0x0

// IEN - Interrupt enable register.
#define VERA_IEN_ADDR 0x8
#define VERA_IEN_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VSYNC : 1; // Vertical sync interrupt enable.
    uint32_t LINE : 1; // Line interrupt enable.
    uint32_t SPRCOL : 1; // Sprite collision interrupt enable.
    uint32_t : 29; // reserved
  };
} vera_ien_t;

// IEN.VSYNC - Vertical sync interrupt enable.
#define VERA_IEN_VSYNC_WIDTH 1
#define VERA_IEN_VSYNC_LSB 0
#define VERA_IEN_VSYNC_MASK 0x1
#define VERA_IEN_VSYNC_RESET 0x0

// IEN.LINE - Line interrupt enable.
#define VERA_IEN_LINE_WIDTH 1
#define VERA_IEN_LINE_LSB 1
#define VERA_IEN_LINE_MASK 0x2
#define VERA_IEN_LINE_RESET 0x0

// IEN.SPRCOL - Sprite collision interrupt enable.
#define VERA_IEN_SPRCOL_WIDTH 1
#define VERA_IEN_SPRCOL_LSB 2
#define VERA_IEN_SPRCOL_MASK 0x4
#define VERA_IEN_SPRCOL_RESET 0x0

// ISR - Interrupt status register.
#define VERA_ISR_ADDR 0xc
#define VERA_ISR_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VSYNC : 1; // Vertical sync interrupt status.
    uint32_t LINE : 1; // Line interrupt status.
    uint32_t SPRCOL : 1; // Sprite collision interrupt status.
    uint32_t : 1; // reserved
    uint32_t SPR_COLLISIONS : 4; // Sprite collisions as determined by sprite renderer.
    uint32_t : 24; // reserved
  };
} vera_isr_t;

// ISR.VSYNC - Vertical sync interrupt status.
#define VERA_ISR_VSYNC_WIDTH 1
#define VERA_ISR_VSYNC_LSB 0
#define VERA_ISR_VSYNC_MASK 0x1
#define VERA_ISR_VSYNC_RESET 0x0

// ISR.LINE - Line interrupt status.
#define VERA_ISR_LINE_WIDTH 1
#define VERA_ISR_LINE_LSB 1
#define VERA_ISR_LINE_MASK 0x2
#define VERA_ISR_LINE_RESET 0x0

// ISR.SPRCOL - Sprite collision interrupt status.
#define VERA_ISR_SPRCOL_WIDTH 1
#define VERA_ISR_SPRCOL_LSB 2
#define VERA_ISR_SPRCOL_MASK 0x4
#define VERA_ISR_SPRCOL_RESET 0x0

// ISR.SPR_COLLISIONS - Sprite collisions as determined by sprite renderer.
#define VERA_ISR_SPR_COLLISIONS_WIDTH 4
#define VERA_ISR_SPR_COLLISIONS_LSB 4
#define VERA_ISR_SPR_COLLISIONS_MASK 0xf0
#define VERA_ISR_SPR_COLLISIONS_RESET 0x0

// IRQLINE - Interrupt line register.
#define VERA_IRQLINE_ADDR 0x10
#define VERA_IRQLINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Scanline on which to generate line interrupt.
    uint32_t : 22; // reserved
  };
} vera_irqline_t;

// IRQLINE.VALUE - Scanline on which to generate line interrupt.
#define VERA_IRQLINE_VALUE_WIDTH 10
#define VERA_IRQLINE_VALUE_LSB 0
#define VERA_IRQLINE_VALUE_MASK 0x3ff
#define VERA_IRQLINE_VALUE_RESET 0x0

// SCANLINE - Scanline register
#define VERA_SCANLINE_ADDR 0x14
#define VERA_SCANLINE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Current scanline.
    uint32_t : 22; // reserved
  };
} vera_scanline_t;

// SCANLINE.VALUE - Current scanline.
#define VERA_SCANLINE_VALUE_WIDTH 10
#define VERA_SCANLINE_VALUE_LSB 0
#define VERA_SCANLINE_VALUE_MASK 0x3ff
#define VERA_SCANLINE_VALUE_RESET 0x0

// DC_VIDEO - Display composer video register.
#define VERA_DC_VIDEO_ADDR 0x18
#define VERA_DC_VIDEO_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t OUTPUT_MODE : 2; // Video output mode.
    uint32_t : 2; // reserved
    uint32_t L0_ENABLE : 1; // Enable Layer 0.
    uint32_t L1_ENABLE : 1; // Enable layer 1.
    uint32_t SPR_ENABLE : 1; // Enable sprites.
    uint32_t : 25; // reserved
  };
} vera_dc_video_t;

// DC_VIDEO.OUTPUT_MODE - Video output mode.
#define VERA_DC_VIDEO_OUTPUT_MODE_WIDTH 2
#define VERA_DC_VIDEO_OUTPUT_MODE_LSB 0
#define VERA_DC_VIDEO_OUTPUT_MODE_MASK 0x3
#define VERA_DC_VIDEO_OUTPUT_MODE_RESET 0x0
typedef enum {
    VERA_DC_VIDEO_OUTPUT_MODE_DIS = 0x0, //Video disabled.
    VERA_DC_VIDEO_OUTPUT_MODE_VGA = 0x1, //VGA output enabled.
} vera_dc_video_output_mode_t;

// DC_VIDEO.L0_ENABLE - Enable Layer 0.
#define VERA_DC_VIDEO_L0_ENABLE_WIDTH 1
#define VERA_DC_VIDEO_L0_ENABLE_LSB 4
#define VERA_DC_VIDEO_L0_ENABLE_MASK 0x10
#define VERA_DC_VIDEO_L0_ENABLE_RESET 0x0

// DC_VIDEO.L1_ENABLE - Enable layer 1.
#define VERA_DC_VIDEO_L1_ENABLE_WIDTH 1
#define VERA_DC_VIDEO_L1_ENABLE_LSB 5
#define VERA_DC_VIDEO_L1_ENABLE_MASK 0x20
#define VERA_DC_VIDEO_L1_ENABLE_RESET 0x0

// DC_VIDEO.SPR_ENABLE - Enable sprites.
#define VERA_DC_VIDEO_SPR_ENABLE_WIDTH 1
#define VERA_DC_VIDEO_SPR_ENABLE_LSB 6
#define VERA_DC_VIDEO_SPR_ENABLE_MASK 0x40
#define VERA_DC_VIDEO_SPR_ENABLE_RESET 0x0

// DC_HSCALE - Display composer horizontal scale register.
#define VERA_DC_HSCALE_ADDR 0x20
#define VERA_DC_HSCALE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // the horizonal fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
    uint32_t : 24; // reserved
  };
} vera_dc_hscale_t;

// DC_HSCALE.VALUE - the horizonal fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
#define VERA_DC_HSCALE_VALUE_WIDTH 8
#define VERA_DC_HSCALE_VALUE_LSB 0
#define VERA_DC_HSCALE_VALUE_MASK 0xff
#define VERA_DC_HSCALE_VALUE_RESET 0x0

// DC_VSCALE - Display composer vertical scale register.
#define VERA_DC_VSCALE_ADDR 0x24
#define VERA_DC_VSCALE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 8; // the vertical fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
    uint32_t : 24; // reserved
  };
} vera_dc_vscale_t;

// DC_VSCALE.VALUE - the vertical fractional scaling factor of the active part of the display. Setting this value to 128 will output 1 output pixel for every input pixel. Setting this to 64 will output 2 output pixels for every input pixel.
#define VERA_DC_VSCALE_VALUE_WIDTH 8
#define VERA_DC_VSCALE_VALUE_LSB 0
#define VERA_DC_VSCALE_VALUE_MASK 0xff
#define VERA_DC_VSCALE_VALUE_RESET 0x0

// DC_HSTART - Display composer horizontal start register.
#define VERA_DC_HSTART_ADDR 0x28
#define VERA_DC_HSTART_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Horizontal start of active part of screen in 640x480 space.
    uint32_t : 22; // reserved
  };
} vera_dc_hstart_t;

// DC_HSTART.VALUE - Horizontal start of active part of screen in 640x480 space.
#define VERA_DC_HSTART_VALUE_WIDTH 10
#define VERA_DC_HSTART_VALUE_LSB 0
#define VERA_DC_HSTART_VALUE_MASK 0x3ff
#define VERA_DC_HSTART_VALUE_RESET 0x0

// DC_HSTOP - Display compser horizontal stop register.
#define VERA_DC_HSTOP_ADDR 0x2c
#define VERA_DC_HSTOP_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Horizontal stop of active part of screen in 640x480 space.
    uint32_t : 22; // reserved
  };
} vera_dc_hstop_t;

// DC_HSTOP.VALUE - Horizontal stop of active part of screen in 640x480 space.
#define VERA_DC_HSTOP_VALUE_WIDTH 10
#define VERA_DC_HSTOP_VALUE_LSB 0
#define VERA_DC_HSTOP_VALUE_MASK 0x3ff
#define VERA_DC_HSTOP_VALUE_RESET 0x0

// DC_VSTART - Display composer vertical start register.
#define VERA_DC_VSTART_ADDR 0x30
#define VERA_DC_VSTART_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Vertical start of active part of screen in 640x480 space.
    uint32_t : 22; // reserved
  };
} vera_dc_vstart_t;

// DC_VSTART.VALUE - Vertical start of active part of screen in 640x480 space.
#define VERA_DC_VSTART_VALUE_WIDTH 10
#define VERA_DC_VSTART_VALUE_LSB 0
#define VERA_DC_VSTART_VALUE_MASK 0x3ff
#define VERA_DC_VSTART_VALUE_RESET 0x0

// DC_VSTOP - Display composer vertical stop register.
#define VERA_DC_VSTOP_ADDR 0x34
#define VERA_DC_VSTOP_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 10; // Vertical stop of active part of screen in 640x480 space.
    uint32_t : 22; // reserved
  };
} vera_dc_vstop_t;

// DC_VSTOP.VALUE - Vertical stop of active part of screen in 640x480 space.
#define VERA_DC_VSTOP_VALUE_WIDTH 10
#define VERA_DC_VSTOP_VALUE_LSB 0
#define VERA_DC_VSTOP_VALUE_MASK 0x3ff
#define VERA_DC_VSTOP_VALUE_RESET 0x0

// L0_CONFIG - Layer 0 Configuration regiser.
#define VERA_L0_CONFIG_ADDR 0x40
#define VERA_L0_CONFIG_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t COLOR_DEPTH : 2; // Number of bits pers pixel to encode color information.
    uint32_t BITMAP_MODE : 1; // 1 selects bitmap mode, 0 selects tile mode.
    uint32_t T256C : 1; // When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
    uint32_t MAP_WIDTH : 2; // Tile map width.
    uint32_t MAP_HEIGHT : 2; // Tile map height.
    uint32_t : 24; // reserved
  };
} vera_l0_config_t;

// L0_CONFIG.COLOR_DEPTH - Number of bits pers pixel to encode color information.
#define VERA_L0_CONFIG_COLOR_DEPTH_WIDTH 2
#define VERA_L0_CONFIG_COLOR_DEPTH_LSB 0
#define VERA_L0_CONFIG_COLOR_DEPTH_MASK 0x3
#define VERA_L0_CONFIG_COLOR_DEPTH_RESET 0x0
typedef enum {
    VERA_L0_CONFIG_COLOR_DEPTH_ONE_BPP = 0x0, //1 bpp.
    VERA_L0_CONFIG_COLOR_DEPTH_TWO_BPP = 0x1, //2 bpp.
    VERA_L0_CONFIG_COLOR_DEPTH_FOUR_BPP = 0x2, //4 bpp.
    VERA_L0_CONFIG_COLOR_DEPTH_EIGHT_BPP = 0x3, //8 bpp.
} vera_l0_config_color_depth_t;

// L0_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
#define VERA_L0_CONFIG_BITMAP_MODE_WIDTH 1
#define VERA_L0_CONFIG_BITMAP_MODE_LSB 2
#define VERA_L0_CONFIG_BITMAP_MODE_MASK 0x4
#define VERA_L0_CONFIG_BITMAP_MODE_RESET 0x0

// L0_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
#define VERA_L0_CONFIG_T256C_WIDTH 1
#define VERA_L0_CONFIG_T256C_LSB 3
#define VERA_L0_CONFIG_T256C_MASK 0x8
#define VERA_L0_CONFIG_T256C_RESET 0x0

// L0_CONFIG.MAP_WIDTH - Tile map width.
#define VERA_L0_CONFIG_MAP_WIDTH_WIDTH 2
#define VERA_L0_CONFIG_MAP_WIDTH_LSB 4
#define VERA_L0_CONFIG_MAP_WIDTH_MASK 0x30
#define VERA_L0_CONFIG_MAP_WIDTH_RESET 0x0
typedef enum {
    VERA_L0_CONFIG_MAP_WIDTH_TILES_32 = 0x0, //32 tiles wide.
    VERA_L0_CONFIG_MAP_WIDTH_TILES_64 = 0x1, //64 tiles wide.
    VERA_L0_CONFIG_MAP_WIDTH_TILES_128 = 0x2, //128 tiles wide.
    VERA_L0_CONFIG_MAP_WIDTH_TILES_256 = 0x3, //256 tiles wide.
} vera_l0_config_map_width_t;

// L0_CONFIG.MAP_HEIGHT - Tile map height.
#define VERA_L0_CONFIG_MAP_HEIGHT_WIDTH 2
#define VERA_L0_CONFIG_MAP_HEIGHT_LSB 6
#define VERA_L0_CONFIG_MAP_HEIGHT_MASK 0xc0
#define VERA_L0_CONFIG_MAP_HEIGHT_RESET 0x0
typedef enum {
    VERA_L0_CONFIG_MAP_HEIGHT_TILES_32 = 0x0, //32 tiles high.
    VERA_L0_CONFIG_MAP_HEIGHT_TILES_64 = 0x1, //64 tiles high.
    VERA_L0_CONFIG_MAP_HEIGHT_TILES_128 = 0x2, //128 tiles high.
    VERA_L0_CONFIG_MAP_HEIGHT_TILES_256 = 0x3, //256 tiles high.
} vera_l0_config_map_height_t;

// L0_MAPBASE - Layer 0 map base register.
#define VERA_L0_MAPBASE_ADDR 0x44
#define VERA_L0_MAPBASE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t ADDR_16_9 : 8; // Bits 16:9 of the base address of the tile map.
    uint32_t : 24; // reserved
  };
} vera_l0_mapbase_t;

// L0_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
#define VERA_L0_MAPBASE_ADDR_16_9_WIDTH 8
#define VERA_L0_MAPBASE_ADDR_16_9_LSB 0
#define VERA_L0_MAPBASE_ADDR_16_9_MASK 0xff
#define VERA_L0_MAPBASE_ADDR_16_9_RESET 0x0

// L0_TILEBASE - Layer 0 tile base register.
#define VERA_L0_TILEBASE_ADDR 0x48
#define VERA_L0_TILEBASE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t TILE_BITMAP_WIDTH : 1; // Tile or Bitmap width.
    uint32_t TILE_HEIGHT : 1; // Tile height.
    uint32_t TILE_BASE_ADDR_16_11 : 6; // Bits 16:11 of the base address of the tile data.
    uint32_t : 24; // reserved
  };
} vera_l0_tilebase_t;

// L0_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
#define VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_WIDTH 1
#define VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_LSB 0
#define VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_MASK 0x1
#define VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_RESET 0x0
typedef enum {
    VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_TILE_BITMAP_W_8_320 = 0x0, //8 pixel tile width, 320 pixels bitmap width.
    VERA_L0_TILEBASE_TILE_BITMAP_WIDTH_TILE_BITMAP_W_16_640 = 0x1, //16 pixel tile width, 640 pixels bitmap width.
} vera_l0_tilebase_tile_bitmap_width_t;

// L0_TILEBASE.TILE_HEIGHT - Tile height.
#define VERA_L0_TILEBASE_TILE_HEIGHT_WIDTH 1
#define VERA_L0_TILEBASE_TILE_HEIGHT_LSB 1
#define VERA_L0_TILEBASE_TILE_HEIGHT_MASK 0x2
#define VERA_L0_TILEBASE_TILE_HEIGHT_RESET 0x0
typedef enum {
    VERA_L0_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_8 = 0x0, //8 pixel tile height.
    VERA_L0_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_16 = 0x1, //16 pixel tile height.
} vera_l0_tilebase_tile_height_t;

// L0_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
#define VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH 6
#define VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_LSB 2
#define VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_MASK 0xfc
#define VERA_L0_TILEBASE_TILE_BASE_ADDR_16_11_RESET 0x0

// L0_HSCROLL - Layer 0 horizontal scroll register.
#define VERA_L0_HSCROLL_ADDR 0x50
#define VERA_L0_HSCROLL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t HSCROLL_7_0 : 8; // Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
    uint32_t HSCROLL_11_8_PAL_OFFSET : 4; // In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
    uint32_t : 20; // reserved
  };
} vera_l0_hscroll_t;

// L0_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
#define VERA_L0_HSCROLL_HSCROLL_7_0_WIDTH 8
#define VERA_L0_HSCROLL_HSCROLL_7_0_LSB 0
#define VERA_L0_HSCROLL_HSCROLL_7_0_MASK 0xff
#define VERA_L0_HSCROLL_HSCROLL_7_0_RESET 0x0

// L0_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
#define VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH 4
#define VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB 8
#define VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK 0xf00
#define VERA_L0_HSCROLL_HSCROLL_11_8_PAL_OFFSET_RESET 0x0

// L0_VSCROLL - Layer 0 vertical scroll register.
#define VERA_L0_VSCROLL_ADDR 0x54
#define VERA_L0_VSCROLL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 12; // Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
    uint32_t : 20; // reserved
  };
} vera_l0_vscroll_t;

// L0_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
#define VERA_L0_VSCROLL_VALUE_WIDTH 12
#define VERA_L0_VSCROLL_VALUE_LSB 0
#define VERA_L0_VSCROLL_VALUE_MASK 0xfff
#define VERA_L0_VSCROLL_VALUE_RESET 0x0

// L1_CONFIG - Layer 1 Configuration regiser.
#define VERA_L1_CONFIG_ADDR 0x80
#define VERA_L1_CONFIG_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t COLOR_DEPTH : 2; // Number of bits pers pixel to encode color information.
    uint32_t BITMAP_MODE : 1; // 1 selects bitmap mode, 0 selects tile mode.
    uint32_t T256C : 1; // When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
    uint32_t MAP_WIDTH : 2; // Tile map width.
    uint32_t MAP_HEIGHT : 2; // Tile map height.
    uint32_t : 24; // reserved
  };
} vera_l1_config_t;

// L1_CONFIG.COLOR_DEPTH - Number of bits pers pixel to encode color information.
#define VERA_L1_CONFIG_COLOR_DEPTH_WIDTH 2
#define VERA_L1_CONFIG_COLOR_DEPTH_LSB 0
#define VERA_L1_CONFIG_COLOR_DEPTH_MASK 0x3
#define VERA_L1_CONFIG_COLOR_DEPTH_RESET 0x0
typedef enum {
    VERA_L1_CONFIG_COLOR_DEPTH_ONE_BPP = 0x0, //1 bpp.
    VERA_L1_CONFIG_COLOR_DEPTH_TWO_BPP = 0x1, //2 bpp.
    VERA_L1_CONFIG_COLOR_DEPTH_FOUR_BPP = 0x2, //4 bpp.
    VERA_L1_CONFIG_COLOR_DEPTH_EIGHT_BPP = 0x3, //8 bpp.
} vera_l1_config_color_depth_t;

// L1_CONFIG.BITMAP_MODE - 1 selects bitmap mode, 0 selects tile mode.
#define VERA_L1_CONFIG_BITMAP_MODE_WIDTH 1
#define VERA_L1_CONFIG_BITMAP_MODE_LSB 2
#define VERA_L1_CONFIG_BITMAP_MODE_MASK 0x4
#define VERA_L1_CONFIG_BITMAP_MODE_RESET 0x0

// L1_CONFIG.T256C - When set, 1 bpp tile mode tiles use 16-color foreground and background. When clear, they use 256-color foreground. Not relevant in other modes.
#define VERA_L1_CONFIG_T256C_WIDTH 1
#define VERA_L1_CONFIG_T256C_LSB 3
#define VERA_L1_CONFIG_T256C_MASK 0x8
#define VERA_L1_CONFIG_T256C_RESET 0x0

// L1_CONFIG.MAP_WIDTH - Tile map width.
#define VERA_L1_CONFIG_MAP_WIDTH_WIDTH 2
#define VERA_L1_CONFIG_MAP_WIDTH_LSB 4
#define VERA_L1_CONFIG_MAP_WIDTH_MASK 0x30
#define VERA_L1_CONFIG_MAP_WIDTH_RESET 0x0
typedef enum {
    VERA_L1_CONFIG_MAP_WIDTH_TILES_32 = 0x0, //32 tiles wide.
    VERA_L1_CONFIG_MAP_WIDTH_TILES_64 = 0x1, //64 tiles wide.
    VERA_L1_CONFIG_MAP_WIDTH_TILES_128 = 0x2, //128 tiles wide.
    VERA_L1_CONFIG_MAP_WIDTH_TILES_256 = 0x3, //256 tiles wide.
} vera_l1_config_map_width_t;

// L1_CONFIG.MAP_HEIGHT - Tile map height.
#define VERA_L1_CONFIG_MAP_HEIGHT_WIDTH 2
#define VERA_L1_CONFIG_MAP_HEIGHT_LSB 6
#define VERA_L1_CONFIG_MAP_HEIGHT_MASK 0xc0
#define VERA_L1_CONFIG_MAP_HEIGHT_RESET 0x0
typedef enum {
    VERA_L1_CONFIG_MAP_HEIGHT_TILES_32 = 0x0, //32 tiles high.
    VERA_L1_CONFIG_MAP_HEIGHT_TILES_64 = 0x1, //64 tiles high.
    VERA_L1_CONFIG_MAP_HEIGHT_TILES_128 = 0x2, //128 tiles high.
    VERA_L1_CONFIG_MAP_HEIGHT_TILES_256 = 0x3, //256 tiles high.
} vera_l1_config_map_height_t;

// L1_MAPBASE - Layer 1 map base register.
#define VERA_L1_MAPBASE_ADDR 0x84
#define VERA_L1_MAPBASE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t ADDR_16_9 : 8; // Bits 16:9 of the base address of the tile map.
    uint32_t : 24; // reserved
  };
} vera_l1_mapbase_t;

// L1_MAPBASE.ADDR_16_9 - Bits 16:9 of the base address of the tile map.
#define VERA_L1_MAPBASE_ADDR_16_9_WIDTH 8
#define VERA_L1_MAPBASE_ADDR_16_9_LSB 0
#define VERA_L1_MAPBASE_ADDR_16_9_MASK 0xff
#define VERA_L1_MAPBASE_ADDR_16_9_RESET 0x0

// L1_TILEBASE - Layer 1 tile base register.
#define VERA_L1_TILEBASE_ADDR 0x88
#define VERA_L1_TILEBASE_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t TILE_BITMAP_WIDTH : 1; // Tile or Bitmap width.
    uint32_t TILE_HEIGHT : 1; // Tile height.
    uint32_t TILE_BASE_ADDR_16_11 : 6; // Bits 16:11 of the base address of the tile data.
    uint32_t : 24; // reserved
  };
} vera_l1_tilebase_t;

// L1_TILEBASE.TILE_BITMAP_WIDTH - Tile or Bitmap width.
#define VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_WIDTH 1
#define VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_LSB 0
#define VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_MASK 0x1
#define VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_RESET 0x0
typedef enum {
    VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_TILE_BITMAP_W_8_320 = 0x0, //8 pixel tile width, 320 pixels bitmap width.
    VERA_L1_TILEBASE_TILE_BITMAP_WIDTH_TILE_BITMAP_W_16_640 = 0x1, //16 pixel tile width, 640 pixels bitmap width.
} vera_l1_tilebase_tile_bitmap_width_t;

// L1_TILEBASE.TILE_HEIGHT - Tile height.
#define VERA_L1_TILEBASE_TILE_HEIGHT_WIDTH 1
#define VERA_L1_TILEBASE_TILE_HEIGHT_LSB 1
#define VERA_L1_TILEBASE_TILE_HEIGHT_MASK 0x2
#define VERA_L1_TILEBASE_TILE_HEIGHT_RESET 0x0
typedef enum {
    VERA_L1_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_8 = 0x0, //8 pixel tile height.
    VERA_L1_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_16 = 0x1, //16 pixel tile height.
} vera_l1_tilebase_tile_height_t;

// L1_TILEBASE.TILE_BASE_ADDR_16_11 - Bits 16:11 of the base address of the tile data.
#define VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_WIDTH 6
#define VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_LSB 2
#define VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_MASK 0xfc
#define VERA_L1_TILEBASE_TILE_BASE_ADDR_16_11_RESET 0x0

// L1_HSCROLL - Layer 1 horizontal scroll register.
#define VERA_L1_HSCROLL_ADDR 0x90
#define VERA_L1_HSCROLL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t HSCROLL_7_0 : 8; // Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
    uint32_t HSCROLL_11_8_PAL_OFFSET : 4; // In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
    uint32_t : 20; // reserved
  };
} vera_l1_hscroll_t;

// L1_HSCROLL.HSCROLL_7_0 - Specifies bits 7:0 of the horizontal scroll offset. Increasing the value will cause the picture to move left, decreasing will cause the picture to move right.
#define VERA_L1_HSCROLL_HSCROLL_7_0_WIDTH 8
#define VERA_L1_HSCROLL_HSCROLL_7_0_LSB 0
#define VERA_L1_HSCROLL_HSCROLL_7_0_MASK 0xff
#define VERA_L1_HSCROLL_HSCROLL_7_0_RESET 0x0

// L1_HSCROLL.HSCROLL_11_8_PAL_OFFSET - In Tile Mode, specifies bits 11:8 of the horizontal scroll offset. In Bitmap Mode, specifies the palette offset of the bitmap colors.
#define VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_WIDTH 4
#define VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_LSB 8
#define VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_MASK 0xf00
#define VERA_L1_HSCROLL_HSCROLL_11_8_PAL_OFFSET_RESET 0x0

// L1_VSCROLL - Layer 1 vertical scroll register.
#define VERA_L1_VSCROLL_ADDR 0x94
#define VERA_L1_VSCROLL_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint32_t VALUE : 12; // Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
    uint32_t : 20; // reserved
  };
} vera_l1_vscroll_t;

// L1_VSCROLL.VALUE - Specifies the vertical scroll offset. A value between 0 and 4095 can be used. Increasing the value will cause the picture to move up, decreasing will cause the picture to move down.
#define VERA_L1_VSCROLL_VALUE_WIDTH 12
#define VERA_L1_VSCROLL_VALUE_LSB 0
#define VERA_L1_VSCROLL_VALUE_MASK 0xfff
#define VERA_L1_VSCROLL_VALUE_RESET 0x0


// Register map structure
typedef struct {
    union {
        __IO uint32_t CTRL; // Control register.
        __IO vera_ctrl_t CTRL_bf; // Bit access for CTRL register
    };
    union {
        __IO uint32_t DC_BORDER; // Display composer border register.
        __IO vera_dc_border_t DC_BORDER_bf; // Bit access for DC_BORDER register
    };
    union {
        __IO uint32_t IEN; // Interrupt enable register.
        __IO vera_ien_t IEN_bf; // Bit access for IEN register
    };
    union {
        __IO uint32_t ISR; // Interrupt status register.
        __IO vera_isr_t ISR_bf; // Bit access for ISR register
    };
    union {
        __IO uint32_t IRQLINE; // Interrupt line register.
        __IO vera_irqline_t IRQLINE_bf; // Bit access for IRQLINE register
    };
    union {
        __I uint32_t SCANLINE; // Scanline register
        __I vera_scanline_t SCANLINE_bf; // Bit access for SCANLINE register
    };
    union {
        __IO uint32_t DC_VIDEO; // Display composer video register.
        __IO vera_dc_video_t DC_VIDEO_bf; // Bit access for DC_VIDEO register
    };
    __IO uint32_t RESERVED0[1];
    union {
        __IO uint32_t DC_HSCALE; // Display composer horizontal scale register.
        __IO vera_dc_hscale_t DC_HSCALE_bf; // Bit access for DC_HSCALE register
    };
    union {
        __IO uint32_t DC_VSCALE; // Display composer vertical scale register.
        __IO vera_dc_vscale_t DC_VSCALE_bf; // Bit access for DC_VSCALE register
    };
    union {
        __IO uint32_t DC_HSTART; // Display composer horizontal start register.
        __IO vera_dc_hstart_t DC_HSTART_bf; // Bit access for DC_HSTART register
    };
    union {
        __IO uint32_t DC_HSTOP; // Display compser horizontal stop register.
        __IO vera_dc_hstop_t DC_HSTOP_bf; // Bit access for DC_HSTOP register
    };
    union {
        __IO uint32_t DC_VSTART; // Display composer vertical start register.
        __IO vera_dc_vstart_t DC_VSTART_bf; // Bit access for DC_VSTART register
    };
    union {
        __IO uint32_t DC_VSTOP; // Display composer vertical stop register.
        __IO vera_dc_vstop_t DC_VSTOP_bf; // Bit access for DC_VSTOP register
    };
    __IO uint32_t RESERVED1[2];
    union {
        __IO uint32_t L0_CONFIG; // Layer 0 Configuration regiser.
        __IO vera_l0_config_t L0_CONFIG_bf; // Bit access for L0_CONFIG register
    };
    union {
        __IO uint32_t L0_MAPBASE; // Layer 0 map base register.
        __IO vera_l0_mapbase_t L0_MAPBASE_bf; // Bit access for L0_MAPBASE register
    };
    union {
        __IO uint32_t L0_TILEBASE; // Layer 0 tile base register.
        __IO vera_l0_tilebase_t L0_TILEBASE_bf; // Bit access for L0_TILEBASE register
    };
    __IO uint32_t RESERVED2[1];
    union {
        __IO uint32_t L0_HSCROLL; // Layer 0 horizontal scroll register.
        __IO vera_l0_hscroll_t L0_HSCROLL_bf; // Bit access for L0_HSCROLL register
    };
    union {
        __IO uint32_t L0_VSCROLL; // Layer 0 vertical scroll register.
        __IO vera_l0_vscroll_t L0_VSCROLL_bf; // Bit access for L0_VSCROLL register
    };
    __IO uint32_t RESERVED3[10];
    union {
        __IO uint32_t L1_CONFIG; // Layer 1 Configuration regiser.
        __IO vera_l1_config_t L1_CONFIG_bf; // Bit access for L1_CONFIG register
    };
    union {
        __IO uint32_t L1_MAPBASE; // Layer 1 map base register.
        __IO vera_l1_mapbase_t L1_MAPBASE_bf; // Bit access for L1_MAPBASE register
    };
    union {
        __IO uint32_t L1_TILEBASE; // Layer 1 tile base register.
        __IO vera_l1_tilebase_t L1_TILEBASE_bf; // Bit access for L1_TILEBASE register
    };
    __IO uint32_t RESERVED4[1];
    union {
        __IO uint32_t L1_HSCROLL; // Layer 1 horizontal scroll register.
        __IO vera_l1_hscroll_t L1_HSCROLL_bf; // Bit access for L1_HSCROLL register
    };
    union {
        __IO uint32_t L1_VSCROLL; // Layer 1 vertical scroll register.
        __IO vera_l1_vscroll_t L1_VSCROLL_bf; // Bit access for L1_VSCROLL register
    };
} vera_t;

#define VERA ((vera_t*)(VERA_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __VERA_REGS_H */
