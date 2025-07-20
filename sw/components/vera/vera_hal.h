#ifndef VERA_HAL_H
#define VERA_HAL_H

#include "memmap.h"
#include "vera_regs.h"

#ifdef __cplusplus
extern "C" {
#endif

//Indication of an output parameter
#define VERA_OUT
//Indication of an input parameters
#define VERA_IN

#define VERA_IRQ_VSYNC VERA_IEN_VSYNC_MASK
#define VERA_IRQ_LINE VERA_IEN_LINE_MASK
#define VERA_IRQ_SPRCOL VERA_IEN_SPRCOL_MASK

#define VERA_SCANLINE_VISIBLE_MAX 479
#define VERA_SCANLINE_MAX 524

#define VERA_HSTOP_MAX 1023
#define VERA_VSTOP_MAX 1023

#define VERA_NUM_LAYERS 2

#define VERA_NUM_SPRITE_BANKS 2
#define VERA_NUM_SPRITES_IN_BANK 64 //Number of sprites in one sprite bank)
#define VERA_MAX_SPRITE_ID 127
#define VERA_SPR_ID_ALLOC_FAILED (~0U)

// Vera default color palette.
// The first 16 color are the C64 color palette.
#define VERA_COLOR_BLACK 0
#define VERA_COLOR_WHITE 1
#define VERA_COLOR_RED 2
#define VERA_COLOR_CYAN 3
#define VERA_COLOR_PURPLE 4
#define VERA_COLOR_GREEN 5
#define VERA_COLOR_BLUE 6
#define VERA_COLOR_YELLOW 7
#define VERA_COLOR_ORANGE 8
#define VERA_COLOR_BROWN 9
#define VERA_COLOR_LIGHT_RED 10
#define VERA_COLOR_DARK_GREY 11
#define VERA_COLOR_GREY 12
#define VERA_COLOR_LIGHT_GREEN 13
#define VERA_COLOR_LIGHT_BLUE 14
#define VERA_COLOR_LIGHT_GREY 15

// 16-31 are grayscale values.
#define VERA_COLOR_GRAYSCALE_0 16
#define VERA_COLOR_GRAYSCALE_1 17
#define VERA_COLOR_GRAYSCALE_2 18
#define VERA_COLOR_GRAYSCALE_3 19
#define VERA_COLOR_GRAYSCALE_4 20
#define VERA_COLOR_GRAYSCALE_5 21
#define VERA_COLOR_GRAYSCALE_6 22
#define VERA_COLOR_GRAYSCALE_7 23
#define VERA_COLOR_GRAYSCALE_8 24
#define VERA_COLOR_GRAYSCALE_9 25
#define VERA_COLOR_GRAYSCALE_10 26
#define VERA_COLOR_GRAYSCALE_11 27
#define VERA_COLOR_GRAYSCALE_12 28
#define VERA_COLOR_GRAYSCALE_13 29
#define VERA_COLOR_GRAYSCALE_14 30
#define VERA_COLOR_GRAYSCALE_15 31

//An unsigned 1.7 fixed point type
typedef uint8_t vera_ufix_1_7_t;

//Supported map width and height values.
typedef enum { VERA_MAP_SZ_32=32, VERA_MAP_SZ_64=64, VERA_MAP_SZ_128=128, VERA_MAP_SZ_256=256 } Vera_map_size_t;

//Supported tile width and height values.
typedef enum { VERA_TILE_SZ_8=8, VERA_TILE_SZ_16=16, VERA_TILE_SZ_32=32, VERA_TILE_SZ_64=64 } Vera_tile_size_t;

//Supported bitmap widths
typedef enum { VERA_BITMAP_WIDTH_320=320, VERA_BITMAP_WIDTH_640=640 } Vera_bitmap_width_t;

//Supported color depths.
typedef enum { VERA_BPP_1=1, VERA_BPP_2=2, VERA_BPP_4=4, VERA_BPP_8=8 } Vera_bpp_t;

//Supported map types
typedef enum { VERA_MAP_TYPE_TXT16, VERA_MAP_TYPE_TXT256, VERA_MAP_TYPE_TILE } Vera_map_type_t;

//Supported Layer modes
typedef enum { VERA_LAYER_MODE_TILE, VERA_LAYER_MODE_BITMAP } Vera_layer_mode_t;

//Supported Layers
typedef enum { VERA_L0, VERA_L1 } Vera_layer_t;

// Supported Sprite Z-depth values
// VERA_SPR_Z_DIS: Sprite Disabled.
// VERA_SPR_Z_BG_L0: Between Background and L0.
// VERA_SPR_Z_L0_L1: Between L0 and L1,
// VERA_SPR_Z_L1: In front of L1.
typedef enum { VERA_SPR_Z_DIS=0, VERA_SPR_Z_BG_L0=1, VERA_SPR_Z_L0_L1=2, VERA_SPR_Z_L1=3 } Vera_z_depth_t;

// Sprite attribute bank ids
typedef enum { VERA_SPR_BANK_0=0, VERA_SPR_BANK_1=1 } Vera_sprite_bank_t;

//Screen boundaries structure
typedef struct {
  uint32_t hstart; //Range 0..hstop-1;
  uint32_t hstop; //Range hstart+1..HSTOP_MAX;
  uint32_t vstart; //Range 0..vstop-1
  uint32_t vstop; //Range vsart+1..VSTOP_MAX;
} Vera_screen_boundaries_t;

//Tile mode 1 bpp (16 color textmode) tile map entry type.
typedef struct {
  uint16_t chr : 8;
  uint16_t fg : 4;
  uint16_t bg : 4;
} Vera_textmap_entry_16_t;

//Tile mode 1 bpp (256 color textmode) tile map entry type.
typedef struct {
  uint16_t chr : 8;
  uint16_t fg : 8;
} Vera_textmap_entry_256_t;

//Tile mode 2/4/8bpp tile map entry type.
// The color index of tile pixels is modified by the palette offset using the following logic:
// - Color index 0 (transparent) and 16-255 are unmodified.
// - Color index 1-15 is modified by adding 16 x palette offset.
typedef struct {
  uint16_t tile : 10;
  uint16_t hflip : 1;
  uint16_t vflip : 1;
  uint16_t pal_offset : 4;
} Vera_tilemap_entry_t;

//A 16-bit tile map entry, an overlay of any of the three prior structs.
typedef union {
  uint16_t UINT16;
  Vera_textmap_entry_16_t txt16;
  Vera_textmap_entry_256_t txt256;
  Vera_tilemap_entry_t tile;
} Vera_map_entry_u;

//A tile map structure holding the map base address, its width, height, and type.
typedef struct {
  uint8_t *map_base;
  Vera_map_size_t width;
  Vera_map_size_t height;
  Vera_map_type_t map_type;
} Vera_map_t;

//A tile set structure holding the tile set's base address, the tile's dimensions,
//the color depth, and number of tiles in the set.
typedef struct {
  uint8_t *tileset_base;
  Vera_tile_size_t width; //For non-sprite tiles only VERA_TILE_SZ_8/16 allowed
  Vera_tile_size_t height; //For non-sprite tiles only VERA_TILE_SZ_8/16 allowed
  Vera_bpp_t bpp;
  uint32_t tilesize_bytes;
  uint32_t num_tiles;
} Vera_tileset_t;

//A bitmap structure holding the bitmap's bas address, dimensions and color
//depth.
typedef struct {
  uint8_t *bitmap_base;
  Vera_bitmap_width_t width;
  uint32_t height;
  Vera_bpp_t bpp;
} Vera_bitmap_t;

typedef struct {
  uint8_t *addr;
  uint16_t x;
  uint16_t y;
  Vera_tile_size_t height;
  Vera_tile_size_t width;
  uint8_t pal_offset;
  uint8_t collision_mask; //Range: 0..15
  Vera_z_depth_t z_depth;
  Vera_bpp_t bpp; //Only values VERA_BPP_4 and VERA_BPP_8 are allowed.
  bool vflip;
  bool hflip;
} Vera_sprite_attrs_t;

//Color Palette entry structure.
typedef struct {
  uint16_t b : 4;
  uint16_t g : 4;
  uint16_t r : 4;
  uint16_t : 4;
} Vera_rgb_t;

typedef union {
  uint16_t UINT16;
  Vera_rgb_t rgb;
} Vera_rgb_u;

// Initialize the VERA HAL. Do this before calling any functions that allocate VRAM.
void vera_hal_init();

// Enable IRQs. The passed in mask will be OR'd with the installed mask.
// @param irq_mask: bitwise OR of VERA_IRQs to enable.
void vera_irqs_enable(uint32_t irq_mask);

// Disable IRQs. The passed in mask will be inverted and  AND'd with the installed mask.
// @param irq_mask: bitwise OR of VERA_IRQs to disable.
void vera_irqs_disable(uint32_t irq_mask);

// Retrieve the enabled IRQs bitmask.
// @return: a bitmask of enabled VERA_IRQs.
uint32_t vera_irqs_enabled();

// Retrieve the active IRQs.
// @return: a bitmask of active VERA_IRQs.
uint32_t vera_irqs_get();

// Acknowledge IRQs.
// @param irq_mask: bitwise OR of VERA_IRQs to acknowledge.
void vera_irqs_ack(uint32_t irq_mask);

// Set the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
// enabled.
// @param scanline: scanline number on which the trigger the line IRQ, must be <= VERA_IRQ_LINE_MAX.
void vera_irqline_set(uint32_t scanline);

// Retrieve the scanline on which the line IRQ will be triggered if VERA_IRQ_LINE is enabled.
// @return: line IRQ scanline number, in range 0..VERA_IRQ_LINE_MAX.
uint32_t vera_irqline_get();

// Retrieve the current scanline.
// @return: the current scanline number, in range 0..VERA_IRQ_LINE_MAX.
uint32_t vera_scanline_get();

// Enable/disable the display.
// @param enable: Set to true to enable the display.
void vera_display_enable(bool enable);

// Check if display is enabled
// @return: true if display is enabled.
bool vera_display_enabled();

// Enable/disable sprite rendering.
// @param enable: Set to true to enable sprite rendering.
void vera_sprites_enable(bool enable);

// Check if sprite rendering is enabled
// @return: true if sprite rendering is enabled.
bool vera_sprites_enabled();

// Set the horizontal fractional scale factor.
// @param scalefactor: unsigned 1.7 fixed point value. 1.0 means 1 output pixel for 1 input pixel. 0.5 means
// 2 output pixels for 1 input pixel (zoom in).
void vera_hscale_set(vera_ufix_1_7_t scalefactor);

// Retrieve the horizontal fractional scale factor.
// @return the scalefactor as an unsigned 1.7 fixed point value.
vera_ufix_1_7_t vera_hscale_get();

// Set the vertical fractional scale factor.
// @param scalefactor: unsigned 1.7 fixed point value. 1.0 means 1 output pixel for 1 input pixel. 0.5 means
// 2 output pixels for 1 input pixel (zoom in).
void vera_vscale_set(vera_ufix_1_7_t scalefactor);

// Retrieve the vertical fractional scale factor.
// @return the scalefactor as an unsigned 1.7 fixed point value.
vera_ufix_1_7_t vera_vscale_get();

// Set the border color
// @param palletteIndex: Color palette index of border color.
void vera_bordercolor_set(uint8_t paletteIndex);

// Get the border color
// @return: Color palette index of the border color.
uint8_t vera_bordercolor_get();

// Set the boundaries of the active screen with the 640x480 space.
// @param boundaries: pointer to screen boundaries structure.
void vera_screen_boundaries_set(VERA_IN Vera_screen_boundaries_t *boundaries);

// Retrieve the boundaries of the active screen with the 640x480 space.
// @output param boundaries: Pointer to screen boundaries structure.
void vera_screen_boundaries_get(VERA_OUT Vera_screen_boundaries_t *boundaries);

//
// Tile Map Functions:
//

// Create a map object of the given dimensions. Store dimensions and map base pointer
// into a map structure.
// @param map_base: Set to 0 to have the HAL allocate a map in VRAM.
// Alternatively, pass in a pointer to the start of a map in VRAM. Make sure
// that map alignment requirements are met.
// @param width: width in tiles: VERA_MAP_SZ_32/64/128/256.
// @param height: height in tiles: VERA_MAP_SZ_32/64/128/256.
// @param map_type: VERA_MAP_TYPE_TXT16/TXT256/TILE.
// @output param map: Pointer to a map struct that will be populated with map base
// address and dimensions.
// @return: true if successful, false if memory allocation failed.
bool vera_map_create(uint8_t *map_base, Vera_map_size_t width, Vera_map_size_t height, Vera_map_type_t map_type, VERA_OUT Vera_map_t *map);

// Release VRAM resources allocated for the given map object.
// @param map: Pointer to map struct for which the memory may be released.
void vera_map_delete(VERA_IN Vera_map_t *map);

// Utility function that computes the amount of VRAM needed to store the given
// map.
// @param map: Pointer to map object for which to compute the size.
// @return: The amount of VRAM required in bytes.
uint32_t vera_compute_map_size(VERA_IN Vera_map_t *map);

// Write an enty in the give tile map.
// @param map: Struct hold base address and dimensions of the tile map.
// @param col: the map column. Range: 0..map width - 1;
// @param row: the map row. Range: 0..map height - 1;
// @param entry: The 16-bit data to write into the entry. The entry layout
// depends on the selected tile mode.
void vera_map_write(VERA_IN Vera_map_t *map, uint32_t col, uint32_t row, Vera_map_entry_u entry);

// Read an entry from the given tile map.
// @param map: Struct hold base address and dimensions of the tile map.
// @param col: the map column. Range: 0..map width - 1;
// @param row: the map row. Range: 0..map height - 1;
// @return: The 16-bit entry value. The layout depends on the selected tile mode.
Vera_map_entry_u vera_map_read(VERA_IN Vera_map_t *map, uint32_t col, uint32_t row);

//
// Tile Set Functions:
//

// Create a tileset object using given parameters. Store parameters and tileset base
// pointer into a tileset structure.
//
// A tileset can be used to represent both tilesets and spritesets.
//
// @param tile_base: Set to 0 to have the HAL allocate memory for a tileset in
// VRAM. Alternatively, pass in a pointer to the start address of a tileset in
// VRAM. Make sure that alignment requirements are met.
// @param width: VERA_TILE_SZ_8/16 for non-sprite tiles. For sprites,
// additionally 32 and 64 are allowed.
// @param height: VERA_TILE_SZ_8/16 for non-sprite tiles. For sprites,
// additionally 32 and 64 are allowed.
// @param bpp: VERA_BPP_1/2/4/8. In case of spritesets, only VERA_BPP_4 and 8 are
// allowed.
// @param num_tiles: Number of tiles in the tileset.
// @output param tileset: Pointer to tileset structure with all fields populated.
// @return: true if successful, false if memory allocation failed.
bool vera_tileset_create(uint8_t *tile_base, Vera_tile_size_t width, Vera_tile_size_t height, Vera_bpp_t bpp, uint32_t num_tiles, VERA_OUT Vera_tileset_t *tileset);

// Release VRAM resources allocated for the given tileset object.
// @param tileset: Pointer to tile struct for which the memory may be released.
void vera_tileset_delete(VERA_IN Vera_tileset_t *tileset);

// Utility function that computes the amount of VRAM needed to store the given tileset.
// @para map: Pointer to tileset object for which to compute the size.
// @return: The amount of VRAM required in bytes.
uint32_t vera_compute_tileset_size(VERA_IN Vera_tileset_t *tileset);

// Get a pointer to a tile in a tileset.
// @param tileset: Pointer to tileset object.
// @param tile_idx: Index of the tile in the tileset. Range
// 0..(tileset.num_tiles-1).
// @return: pointer to the tile's pixel data.
uint8_t* vera_get_tileptr(VERA_IN Vera_tileset_t *tileset, uint32_t tile_idx);

//
// Bitmap Functions:
//

// Create a bitmap object using given parameters. Store parameters and bitmap base
// pointer into a bitmap structure.
// @param bitmap_base: Set to 0 to have the HAL allocate memory in VRAM for the
// bitmap. Alternatively, pass in a pointer to the start address of the bitmap
// in VRAM. Make sure that alignment requirements are met.
// @param width: VERA_BITMAP_WIDTH_320/640
// @param height: in pixels.
// @param bpp: Supported values: VERA_BPP_1/2/4/8.
// @output param bitmap: Pointer to bitmap structure with all fields populated.
// @return: true if successful, false if memory allocation failed.
bool vera_bitmap_create(Vera_bitmap_width_t width, uint32_t height, Vera_bpp_t bpp, VERA_OUT Vera_bitmap_t *bitmap);

// Release VRAM resources allocated for the given bitmap object.
// @param bitmap: Pointer to bitmap struct for which the memory may be released.
void vera_bitmap_delete(VERA_IN Vera_bitmap_t *bitmap);

// Utility function that computes the amount of VRAM needed to store the given
// bitmap.
// @para map: Pointer to bitmap object for which to compute the size.
// @return: The amount of VRAM required in bytes.
uint32_t vera_compute_bitmap_size(VERA_IN Vera_bitmap_t *bitmap);

//
// Layer Configuration:
//

// Enable/disable Layer
// @param layer: VERA_L0 or VERA_L1.
// @param enable: Set to true to enable Layer.
void vera_layer_enable(Vera_layer_t layer, bool enable);

// Check if Layer is enabled
// @param layer: VERA_L0 or VERA_L1.
// @return: true if Layer is enabled.
bool vera_layer_enabled(Vera_layer_t layer);

// Set Layer horizontal scroll offset. Increase/decrease to move picture left/right.
// @param layer: VERA_L0 or VERA_L1.
// @param offset: horizontal scroll offset.
// Range: 0..255 in Bitmap mode, 0..4095 in Tile mode.
void vera_layer_hscroll_set(Vera_layer_t layer, uint32_t offset);

// Get Layer horizontal scroll offset.
// @param layer: VERA_L0 or VERA_L1.
// @return: horizontal scroll offset.
// Range: 0..255 in Bitmap mode, 0..4095 in Tile mode.
uint32_t vera_layer_hscroll_get(Vera_layer_t layer);

// Set Layer vertical scroll offset. Increase/decrease to move picture up/down.
// @param layer: VERA_L0 or VERA_L1.
// @param offset: vertical scroll offset. Range: 0..4095.
void vera_layer_vscroll_set(Vera_layer_t layer, uint32_t offset);

// Get Layer vertical scroll offset.
// @param layer: VERA_L0 or VERA_L1.
// @return: vertical scroll offset. Range: 0..4095.
uint32_t vera_layer_vscroll_get(Vera_layer_t layer);

// Set the Layer map width, height and map type to that of the given map.
// @param layer: VERA_L0 or VERA_L1.
// @param map: Pointer to map object.
void vera_layer_map_props_set(Vera_layer_t layer, VERA_IN Vera_map_t *map);

// Get the Layer's map dimensions and type.
// @param layer: VERA_L0 or VERA_L1.
// @output param: Pointer to width: VERA_MAP_SZ_32/64/128/256. Skipped if
// pointer is 0.
// @output param: Pointer to height: VERA_MAP_SZ_32/64/128/256. Skipped if
// pointer is 0.
// @output param: Pointer to map_type: VERA_MAP_TYPE_TXT16/TXT256/TILE. Skipped
// if pointer is 0.
void vera_layer_map_props_get(Vera_layer_t layer, VERA_OUT Vera_map_size_t *width,
                              VERA_OUT Vera_map_size_t *height, VERA_OUT Vera_map_type_t *map_type);

// Set the Layer map base address to the given map.
// @param layer: VERA_L0 or VERA_L1.
// @param map: Pointer to map object.
void vera_layer_mapbase_set(Vera_layer_t layer, VERA_IN Vera_map_t *map);

// Get the Layer map base pointer
// @param layer: VERA_L0 or VERA_L1.
// @return: Pointer to map base.
uint8_t* vera_layer_mapbase_get(Vera_layer_t layer);

// Put Layer in Tile Mode.
// @param layer: VERA_L0 or VERA_L1.
// @param tileset: the tile modes's bpp, tile width and height are taken from
// the given tileset object.
// Note that the tileset width and heigh must be VERA_TILE_SZ_8 or 16.
void vera_layer_tilemode_set(Vera_layer_t layer,
                             VERA_IN Vera_tileset_t *tileset);

// Get Layer Tile Mode properties. Assumes Tile mode.
// @param layer: VERA_L0 or VERA_L1.
// @output param bpp: pointer to the tile color depth in bpp. Skipped if
// pointeer is 0.
// @output param tile_width: Pointer to tile width: VERA_TILE_SZ_8/16. Skipped
// if pointer is 0.
// @output param tile_height: Pointer to tile height: VERA_TILE_SZ_8/16. Skipped
// if pointer is 0.
void vera_layer_tilemode_get(Vera_layer_t layer,
                           VERA_OUT Vera_bpp_t *bpp,
                           VERA_OUT Vera_tile_size_t *tile_width,
                           VERA_OUT Vera_tile_size_t *tile_height);

// Set the Layer tile base address to the given tileset.
// @param layer: VERA_L0 or VERA_L1.
// @param tileset: Pointer to tileset object.
void vera_layer_tilebase_set(Vera_layer_t layer, VERA_IN Vera_tileset_t *tileset);

// Get the Layer tile base pointer
// @param layer: VERA_L0 or VERA_L1.
// @return: Pointer to tile base.
uint8_t* vera_layer_tilebase_get(Vera_layer_t layer);

// Put Layer in Bitmap Mode.
// @param layer: VERA_L0 or VERA_L1.
// @param bitmap: bitmap mode's bpp, and bitmap width are taken from the given
// bitmap object.
void vera_layer_bitmapmode_set(Vera_layer_t layer,
                               VERA_IN Vera_bitmap_t *bitmap);

// Get Layer in Bitmap Mode properties. Assumes Bitmap mode.
// @param layer: VERA_L0 or VERA_L1.
// @output param bpp: Pointer to the color depth in bpp. Skipped if pointer is
// 0.
// @output param bitmap_width: Pointer to the bitmap width in pixels. Skipped if
// pointer is 0.
void vera_layer_bitmapmode_get(Vera_layer_t layer,
                               VERA_OUT Vera_bpp_t *bpp, VERA_OUT Vera_bitmap_width_t *bitmap_width);

// Set the Layer bitmap base address to the given bitmap
// @param layer: VERA_L0 or VERA_L1.
// @param bitmap: Pointer to bitmap object.
void vera_layer_bitmapbase_set(Vera_layer_t layer, VERA_IN Vera_bitmap_t *bitmap);

// Get the Layer bitmap base pointer
// @param layer: VERA_L0 or VERA_L1.
// @return: Pointer to bitmap base.
uint8_t* vera_layer_bitmapbase_get(Vera_layer_t layer);

// Set Layer bitmap palette offset. Assumes Bitmap mode.
// The color index of bitmap pixels is modified by the palette offset using the following logic:
// - Color index 0 (transparent) and 16-255 are unmodified.
// - Color index 1-15 is modified by adding 16 x palette offset.
// @param layer: VERA_L0 or VERA_L1.
// @param offset: palette offset. Range 0..15.
void vera_layer_bitmap_pal_offset_set(Vera_layer_t layer, uint32_t offset);

// Get Layer bitmap palette offset. Assumes Bitmap mode.
// @param layer: VERA_L0 or VERA_L1.
// @return: palette offset. Range 0..15.
uint32_t vera_layer_bitmap_pal_offset_get(Vera_layer_t layer);

// Retrieve the configured Layer mode.
// @param layer: VERA_L0 or VERA_L1.
// @return: VERA_LAYER_MODE_TILE/BITMAP.
Vera_layer_mode_t vera_layer_mode_get(Vera_layer_t layer);

//
// Palette Functions:
//

// Writes the given RGB triple into the given position in VERA's Palette RAM.
// @param idex: Index into the palette RAM. Range: 0..255.
// @param rgb: packed bitfield struct width 4-bit red/green/blue fields. See
// Vera_rgb_u type definition.
void vera_palette_wr(uint32_t idx, Vera_rgb_u rgb);

//
// Sprite Attribute Functions:
//

// Allocate a block of consecutive sprites IDs.
//
// @param bank: Sprite bank selector: VERA_SPR_BANK_0/1.
// @param num_sprs: Number of sprites to allocated. Range 1..64.
// @return: ID of the first sprite allocated. ~0U if allocation failed.
uint32_t vera_spr_ids_alloc(Vera_sprite_bank_t bank, uint32_t num_sprs);

// Release a block of consecutive sprites IDs.
//
// @param ID of the first sprite in the sprite block to release. Range 0..127.
void vera_spr_ids_free(uint32_t sprite_id);

// Set all sprite attributes for the given sprite
// @param sprite_id: Sprite identifier. Range: 0..127.
// @param sprite: Pointer to Sprite object with all attributes populated.
void vera_spr_attr_set(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite);

// Update just the address dattribute of the given sprite
// @param sprite_id: Sprite identifier. Range: 0..127.
// @param sprite: Take the addr attribute from this object.
void vera_spr_attr_update_addr(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite);

// Update just the X attribute of the given sprite
// @param sprite_id: Sprite identifier. Range: 0..127.
// @param sprite: Take the X attribute from this object.
void vera_spr_attr_update_x(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite);

// Update just the Y attribute of the given sprite
// @param sprite_id: Sprite identifier. Range: 0..127.
// @param sprite: Take the Y attribute from this object.
void vera_spr_attr_update_y(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite);

// Set the active sprite bank
// @param bank: Sprite bank selector: VERA_SPR_BANK_0/1.
void vera_spr_bank_set(Vera_sprite_bank_t bank);

// Get the active sprite bank
// @return the active sprite bank.
Vera_sprite_bank_t vera_spr_bank_get();

//
// Lower-Level VRAM access functions:
//

// VERA VRAM word write
// @param addr: offset in VRAM to write word to.
// @param data: 32-bit word to write.
inline void vera_vram_wr_word(uint32_t addr, uint32_t data) {
  *(volatile uint32_t *)(addr+VERA_VRAM_BASE) = data;
}

// VERA VRAM byte write
// @param addr: offset in VRAM to write byte to.
// @param data: byte to write.
inline void vera_vram_wr_byte(uint32_t addr, uint8_t data) {
  *(volatile uint8_t *)(addr+VERA_VRAM_BASE) = data;
}

// VERA VRAM word read
// @param addr: offset in VRAM of word to read.
inline uint32_t vera_vram_rd_word(uint32_t addr) {
  return (*(volatile uint32_t *)(addr+VERA_VRAM_BASE));
}

// VERA VRAM byte read
// @param addr: offset in VRAM of byte to read.
inline uint8_t vera_vram_rd_byte(uint32_t addr) {
  return (*(volatile uint8_t *)(addr+VERA_VRAM_BASE));
}

//
// Lower-Level VRAM allocator functions:
//
//
// Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
// The 'create' functions above use this function to allocate their resources.
// @param size: the number of bytes to allocate (use one the vera_compute* functions
// above).
// @return: If successful: 2KB-aligned Pointer to allocated block of memory in VRAM. In not successful a
// null pointer is returned.
uint8_t* vera_vram_alloc(uint32_t size);

// Release allocated VRAM memory.
// The 'delete' functions above use this function to release the resources they
// allocated.
// @param mem: Pointer to memory block to free.
void vera_vram_free(uint8_t* mem);

#ifdef __cplusplus
}
#endif
#endif //VERA_HAL_H
