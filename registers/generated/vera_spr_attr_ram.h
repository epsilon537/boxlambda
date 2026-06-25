// Created with Corsair v1.0.4
#ifndef __VERA_SPR_ATTR_RAM_H
#define __VERA_SPR_ATTR_RAM_H

#define __I  volatile const // 'read only' permissions
#define __O  volatile       // 'write only' permissions
#define __IO volatile       // 'read / write' permissions


#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define VERA_SPRITE_ATTR_BASE_ADDR 0x12001000

// MODEADDR - Sprite RAM Mode and VRAM Address Attribute (16-bit).
#define VERA_SPRITE_ATTR_MODEADDR_ADDR 0x0
#define VERA_SPRITE_ATTR_MODEADDR_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint16_t ADDR : 12; // VRAM Address bits 16:5.
    uint16_t : 3; // reserved
    uint16_t MODE : 1; // 0=4BPP mode, 1=8BPP mode.
  };
} vera_sprite_attr_modeaddr_t;

// MODEADDR.ADDR - VRAM Address bits 16:5.
#define VERA_SPRITE_ATTR_MODEADDR_ADDR_WIDTH 12
#define VERA_SPRITE_ATTR_MODEADDR_ADDR_LSB 0
#define VERA_SPRITE_ATTR_MODEADDR_ADDR_MASK 0xfff
#define VERA_SPRITE_ATTR_MODEADDR_ADDR_RESET 0x0

// MODEADDR.MODE - 0=4BPP mode, 1=8BPP mode.
#define VERA_SPRITE_ATTR_MODEADDR_MODE_WIDTH 1
#define VERA_SPRITE_ATTR_MODEADDR_MODE_LSB 15
#define VERA_SPRITE_ATTR_MODEADDR_MODE_MASK 0x8000
#define VERA_SPRITE_ATTR_MODEADDR_MODE_RESET 0x0
typedef enum {
    VERA_SPRITE_ATTR_MODEADDR_MODE_4BPP = 0x0, //4 bits per pixel sprite mode.
    VERA_SPRITE_ATTR_MODEADDR_MODE_8BPP = 0x1, //8 bits per pixel sprite mode.
} vera_sprite_attr_modeaddr_mode_t;

// X - Sprite RAM X Attribute (16-bit).
#define VERA_SPRITE_ATTR_X_ADDR 0x2
#define VERA_SPRITE_ATTR_X_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint16_t VAL : 10; // Sprite X position.
    uint16_t : 6; // reserved
  };
} vera_sprite_attr_x_t;

// X.val - Sprite X position.
#define VERA_SPRITE_ATTR_X_VAL_WIDTH 10
#define VERA_SPRITE_ATTR_X_VAL_LSB 0
#define VERA_SPRITE_ATTR_X_VAL_MASK 0x3ff
#define VERA_SPRITE_ATTR_X_VAL_RESET 0x0

// Y - Sprite RAM Y Attribute (16-bit).
#define VERA_SPRITE_ATTR_Y_ADDR 0x4
#define VERA_SPRITE_ATTR_Y_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint16_t VAL : 10; // Sprite Y position.
    uint16_t : 6; // reserved
  };
} vera_sprite_attr_y_t;

// Y.val - Sprite Y position.
#define VERA_SPRITE_ATTR_Y_VAL_WIDTH 10
#define VERA_SPRITE_ATTR_Y_VAL_LSB 0
#define VERA_SPRITE_ATTR_Y_VAL_MASK 0x3ff
#define VERA_SPRITE_ATTR_Y_VAL_RESET 0x0

// FLAGS - Sprite RAM Flags Attribute (16-bit).
#define VERA_SPRITE_ATTR_FLAGS_ADDR 0x6
#define VERA_SPRITE_ATTR_FLAGS_RESET 0x0
typedef union {
  uint32_t UINT32;
  struct {
    uint16_t HFLIP : 1; // Horizontal Flip
    uint16_t VFLIP : 1; // Vertical Flip
    uint16_t ZDEPTH : 2; // Z Depth
    uint16_t COLMASK : 4; // Collision Mask
    uint16_t PALOFFSET : 4; // Sprite Palette Offset.
    uint16_t WIDTH : 2; // Sprite Width
    uint16_t HEIGHT : 2; // Sprite Height
  };
} vera_sprite_attr_flags_t;

// FLAGS.HFLIP - Horizontal Flip
#define VERA_SPRITE_ATTR_FLAGS_HFLIP_WIDTH 1
#define VERA_SPRITE_ATTR_FLAGS_HFLIP_LSB 0
#define VERA_SPRITE_ATTR_FLAGS_HFLIP_MASK 0x1
#define VERA_SPRITE_ATTR_FLAGS_HFLIP_RESET 0x0

// FLAGS.VFLIP - Vertical Flip
#define VERA_SPRITE_ATTR_FLAGS_VFLIP_WIDTH 1
#define VERA_SPRITE_ATTR_FLAGS_VFLIP_LSB 1
#define VERA_SPRITE_ATTR_FLAGS_VFLIP_MASK 0x2
#define VERA_SPRITE_ATTR_FLAGS_VFLIP_RESET 0x0

// FLAGS.ZDEPTH - Z Depth
#define VERA_SPRITE_ATTR_FLAGS_ZDEPTH_WIDTH 2
#define VERA_SPRITE_ATTR_FLAGS_ZDEPTH_LSB 2
#define VERA_SPRITE_ATTR_FLAGS_ZDEPTH_MASK 0xc
#define VERA_SPRITE_ATTR_FLAGS_ZDEPTH_RESET 0x0
typedef enum {
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_DIS = 0x0, //Sprite disabled.
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_BG_L0 = 0x1, //Between background and L0.
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L0_L1 = 0x2, //Between L0 and L1.
    VERA_SPRITE_ATTR_FLAGS_ZDEPTH_L1 = 0x3, //In front of L1.
} vera_sprite_attr_flags_zdepth_t;

// FLAGS.COLMASK - Collision Mask
#define VERA_SPRITE_ATTR_FLAGS_COLMASK_WIDTH 4
#define VERA_SPRITE_ATTR_FLAGS_COLMASK_LSB 4
#define VERA_SPRITE_ATTR_FLAGS_COLMASK_MASK 0xf0
#define VERA_SPRITE_ATTR_FLAGS_COLMASK_RESET 0x0

// FLAGS.PALOFFSET - Sprite Palette Offset.
#define VERA_SPRITE_ATTR_FLAGS_PALOFFSET_WIDTH 4
#define VERA_SPRITE_ATTR_FLAGS_PALOFFSET_LSB 8
#define VERA_SPRITE_ATTR_FLAGS_PALOFFSET_MASK 0xf00
#define VERA_SPRITE_ATTR_FLAGS_PALOFFSET_RESET 0x0

// FLAGS.WIDTH - Sprite Width
#define VERA_SPRITE_ATTR_FLAGS_WIDTH_WIDTH 2
#define VERA_SPRITE_ATTR_FLAGS_WIDTH_LSB 12
#define VERA_SPRITE_ATTR_FLAGS_WIDTH_MASK 0x3000
#define VERA_SPRITE_ATTR_FLAGS_WIDTH_RESET 0x0
typedef enum {
    VERA_SPRITE_ATTR_FLAGS_WIDTH_W8 = 0x0, //8 pixel sprite width
    VERA_SPRITE_ATTR_FLAGS_WIDTH_W16 = 0x1, //16 pixel sprite width
    VERA_SPRITE_ATTR_FLAGS_WIDTH_W32 = 0x2, //32 pixel sprite width
    VERA_SPRITE_ATTR_FLAGS_WIDTH_W64 = 0x3, //64 pixel sprite width
} vera_sprite_attr_flags_width_t;

// FLAGS.HEIGHT - Sprite Height
#define VERA_SPRITE_ATTR_FLAGS_HEIGHT_WIDTH 2
#define VERA_SPRITE_ATTR_FLAGS_HEIGHT_LSB 14
#define VERA_SPRITE_ATTR_FLAGS_HEIGHT_MASK 0xc000
#define VERA_SPRITE_ATTR_FLAGS_HEIGHT_RESET 0x0
typedef enum {
    VERA_SPRITE_ATTR_FLAGS_HEIGHT_H8 = 0x0, //8 pixel sprite height
    VERA_SPRITE_ATTR_FLAGS_HEIGHT_H16 = 0x1, //16 pixel sprite height
    VERA_SPRITE_ATTR_FLAGS_HEIGHT_H32 = 0x2, //32 pixel sprite height
    VERA_SPRITE_ATTR_FLAGS_HEIGHT_H64 = 0x3, //64 pixel sprite height
} vera_sprite_attr_flags_height_t;


// Register map structure
typedef struct {
    union {
        __O uint16_t MODEADDR; // Sprite RAM Mode and VRAM Address Attribute (16-bit).
        __O vera_sprite_attr_modeaddr_t MODEADDR_bf; // Bit access for MODEADDR register
    };
    union {
        __O uint16_t X; // Sprite RAM X Attribute (16-bit).
        __O vera_sprite_attr_x_t X_bf; // Bit access for X register
    };
    union {
        __O uint16_t Y; // Sprite RAM Y Attribute (16-bit).
        __O vera_sprite_attr_y_t Y_bf; // Bit access for Y register
    };
    union {
        __O uint16_t FLAGS; // Sprite RAM Flags Attribute (16-bit).
        __O vera_sprite_attr_flags_t FLAGS_bf; // Bit access for FLAGS register
    };
} vera_sprite_attr_t;

#define VERA_SPRITE_ATTR ((vera_sprite_attr_t*)(VERA_SPRITE_ATTR_BASE_ADDR))

#ifdef __cplusplus
}
#endif

#endif /* __VERA_SPR_ATTR_RAM_H */
