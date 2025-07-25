#ifndef VERA_HAL_H
#define VERA_HAL_H

#include "memmap.h"
#include "vera_regs.h"

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
#define VERA_NUM_SPRITES (VERA_NUM_SPRITE_BANKS*VERA_NUM_SPRITES_IN_BANK)
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

// Internal:
#define VRAM_BLOCK_SZ_BYTES 2048
#define VRAM_NUM_BLOCKS (VERA_VRAM_SIZE_BYTES/VRAM_BLOCK_SZ_BYTES)
#define VERA_NUM_MAPS_TILESETS_BITMAPS 128

// Forward declarations
class Vera;
typedef struct Vera_layer_regs_t Vera_layer_regs_t;

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

//A tile map class holding the map base address, its width, height, and type.
class Vera_map {
public:

  //@return: pointer to base of map in VRAM.
  inline uint8_t* map_base() { return map_base_;}

  //@return: map width.
  inline Vera_map_size_t width() { return width_;}

  //@return: map height.
  inline Vera_map_size_t height() { return height_;}

  //@return: map type.
  inline Vera_map_type_t map_type() { return map_type_;}

  // Write an entry in the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @param entry: The 16-bit data to write into the entry. The entry layout
  // depends on the selected tile mode.
  void write(uint32_t col, uint32_t row, uint16_t entry);

  // Write an entry in the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @param entry: The txt16 data to write into the entry.
  void write(uint32_t col, uint32_t row, Vera_textmap_entry_16_t entry);

  // Write an entry in the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @param entry: The txt256 data to write into the entry.
  void write(uint32_t col, uint32_t row, Vera_textmap_entry_256_t entry);

  // Write an entry in the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @param entry: The tile data to write into the entry.
  void write(uint32_t col, uint32_t row, Vera_tilemap_entry_t entry);

  // Read an entry from the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @return: The 16-bit entry value. The layout depends on the selected tile mode.
  uint16_t read(uint32_t col, uint32_t row);

  // Read an entry from the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @return: The txt16 entry value.
  Vera_textmap_entry_16_t read_txt16(uint32_t col, uint32_t row);

  // Read an entry from the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @return: The txt256 entry value.
  Vera_textmap_entry_256_t read_txt256(uint32_t col, uint32_t row);

  // Read an entry from the map.
  // @param col: the map column. Range: 0..map width - 1;
  // @param row: the map row. Range: 0..map height - 1;
  // @return: The tilemap entry value.
  Vera_tilemap_entry_t read_tile(uint32_t col, uint32_t row);

private:
  void init_(uint8_t *map_base, Vera_map_size_t width, Vera_map_size_t height, Vera_map_type_t map_type);

  uint8_t* map_base_;
  Vera_map_size_t width_;
  Vera_map_size_t height_;
  Vera_map_type_t map_type_;

  friend class Vera;
};

//A tile set class holding the tile set's base address, the tile's dimensions,
//the color depth, and number of tiles in the set.
class Vera_tileset {
public:

  //@return: pointer to the base of the tileset in VRAM.
  inline uint8_t *tileset_base() { return tileset_base_; }

  //@return: tileset width.
  inline Vera_tile_size_t width() { return width_; }

  //@return: tileset height.
  inline Vera_tile_size_t height() { return height_; }

  //@return: tileset bits-per-pixel.
  inline Vera_bpp_t bpp() { return bpp_; }

  //@return: number of tiles allocated to the tileset.
  inline uint32_t num_tiles() { return num_tiles_; }

  inline uint32_t tilesize_bytes() {return tilesize_bytes_;};

  // Get a pointer to a tile in the tileset.
  // @param tile_idx: Index of the tile in the tileset. Range 0..num_tiles-1.
  // @return: pointer to the tile's pixel data.
  uint8_t* tile(uint32_t tile_idx);

  // Set a pixel in a given tile.
  // @param tile_idx: tile index. Range 0..num_tiles-1.
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @param val: pixel value. Range: 0..bpp-1.
  void pixel_set(uint32_t tile_idx, uint32_t x, uint32_t y, uint8_t val);

  // Get a pixel from a given tile.
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @return: pixel value. Range: 0..bpp-1.
  uint8_t pixel_get(uint32_t tile_idx, uint32_t x, uint32_t y);

private:
  void init_(uint8_t *tileset_base, Vera_tile_size_t width, Vera_tile_size_t height, Vera_bpp_t bpp, uint32_t num_tiles);


  uint8_t * tileset_base_;
  Vera_tile_size_t width_; //For non-sprite tiles only VERA_TILE_SZ_8/16 allowed
  Vera_tile_size_t height_; //For non-sprite tiles only VERA_TILE_SZ_8/16 allowed
  Vera_bpp_t bpp_;
  uint32_t num_tiles_;
  uint32_t tilesize_bytes_;

  friend class Vera;
};

//A bitmap class holding the bitmap's bas address, dimensions and color
//depth.
class Vera_bitmap {
public:
  //@return: pointer to the base of the bitmap in VRAM.
  inline uint8_t *bitmap_base() { return bitmap_base_; }

  //@return: bitmap width.
  inline Vera_bitmap_width_t width() { return width_; }

  //@return: bitmap height.
  inline uint32_t height() { return height_; }

  //@return: tileset bits-per-pixel.
  inline Vera_bpp_t bpp() { return bpp_; }

  // Set a pixel in the bitmap.
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @param val: pixel value. Range: 0..bpp-1.
  void pixel_set(uint32_t x, uint32_t y, uint8_t val);

  // Get a pixel from the bitmap.
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @return: pixel value. Range: 0..bpp-1.
  uint8_t pixel_get(uint32_t x, uint32_t y);

private:
  void init_(uint8_t *bitmap_base, Vera_bitmap_width_t width, uint32_t height, Vera_bpp_t bpp);

  uint8_t * bitmap_base_;
  Vera_bitmap_width_t width_;
  uint32_t height_;
  Vera_bpp_t bpp_;

  friend class Vera;
};

//For internal use.
typedef union Vera_map_tileset_bitmap_u {
  Vera_map map;
  Vera_tileset tileset;
  Vera_bitmap bitmap;

  inline Vera_map_tileset_bitmap_u() {}
  inline  ~Vera_map_tileset_bitmap_u() {}
} Vera_map_tileset_bitmap_u;

//A Sprite class holding a sprite's ID and its attributes.
class Vera_sprite {
public:
  //(Re)initialize the sprite object, maintaining the sprite id.
  void init();

  //@return: sprite ID. Range 0..127.
  inline uint32_t id_get() { return id_; }

  // Set the sprite's tile containing the sprite's pixel data.
  // Attributes width, height and bpp are taken from the tileset.
  // @param tileset: Pointer to the tileset with which this sprite is associate.
  // @param tile_idx: the index of the tile in the tileset containing the sprite's pixel
  // data. Range: 0..tileset.num_tiles.
  void tile_set(Vera_tileset *tileset, uint32_t tile_idx);

  // @output param tileset: reference to the pointer to the tileset with which this sprite is associate.
  // @output param tile_idx: reference to the index of the tile in the tileset containing the sprite's pixel
  // data. Range: 0..tileset.num_tiles.
  void tile(VERA_OUT Vera_tileset* &tileset, VERA_OUT uint32_t& tile_idx);

  // @param x: x-position of the sprite. Range: 0..1023.
  void x_set(uint16_t x);

  // @return: x-position of the sprite. Range: 0..1023.
  inline uint16_t x_get() { return x_; }

  // @param y: y-position of the sprite. Range: 0..1023.
  void y_set(uint16_t y);

  // @return: y-position of the sprite. Range: 0..1023.
  inline uint16_t y_get() { return y_; }

  // @param pal_offset: The palette offset to use for this sprite. Range: 0..15.
  // The color index of tile pixels is modified by the palette offset using the following logic:
  // - Color index 0 (transparent) and 16-255 are unmodified.
  // - Color index 1-15 is modified by adding 16 x palette offset.
  void pal_offset_set(uint8_t pal_offset);

  // @return: The palette offset used. for this sprite
  inline uint8_t pal_offset_get() {
    return pal_offset_;
  }

  // @param collision_mask: Range: 0..15.
  void collision_mask_set(uint8_t collision_mask);

  // @return: collision_mask. Range: 0..15.
  inline uint8_t collision_mask_get() { return collision_mask_; }

  // Set the sprite's z-depth relative to the other layers.
  // @param z-depth: VERA_SPR_Z_DIS/BG_L0/L0_L1/L1.
  void z_depth_set(Vera_z_depth_t z_depth);

  // @return: z-depth: VERA_SPR_Z_DIS/BG_L0/L0_L1/L1.
  inline Vera_z_depth_t z_depth_get() { return z_depth_; }

  // @param hflip: Set to flip sprite horizontally.
  void hflip_set(bool hflip);

  // @return: hflip value.
  inline bool hflip_get() { return hflip_; }

  // @param vflip: Set to flip sprite vertically.
  void vflip_set(bool vflip);

  // @return: vflip value.
  inline bool vflip_get() { return vflip_; }

  // Set a pixel in the sprite
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @param val: pixel value. Range: 0..bpp-1.
  void pixel_set(uint32_t x, uint32_t y, uint8_t val);

  // Get a pixel from the sprite.
  // @param x: pixel x-position. Range: 0..width-1.
  // @param y: pixel y-position. Range: 0..height-1.
  // @return: pixel value. Range: 0..bpp-1.
  uint8_t pixel_get(uint32_t x, uint32_t y);

private:
  inline Vera_sprite() {}

  //Set id service as delayed initialization.
  void set_id_(uint32_t id);

  void set_attr_byte6_();
  void set_attr_byte7_();

  uint32_t id_;
  //Pointer to the sprite's entry in the Sprite Attribute RAM as an array of 4
  //16-bit values.
  uint16_t *attrs_;
  Vera_tileset *tileset_;
  uint32_t tile_idx_;
  uint16_t x_;
  uint16_t y_;
  uint8_t pal_offset_;
  uint8_t collision_mask_; //Range: 0..15
  Vera_z_depth_t z_depth_;
  bool vflip_;
  bool hflip_;

  friend class Vera;
};

//Color Palette entry structure.
typedef struct {
  uint16_t b : 4;
  uint16_t g : 4;
  uint16_t r : 4;
  uint16_t : 4;
} Vera_rgb_t;

class Vera_palette {
public:
  //Write an entry into the palette.
  //@param idx: the palete color index:
  //@param rgb: the RGB tripl
  void write(uint32_t idx, Vera_rgb_t rgb);

  //Write an entry into the palette.
  //@param idx: the palete color index:
  //@param val: the RGB triple as a uint16_t.
  void write(uint32_t idx, uint16_t val);

  //Read the RGB value of a palette entry
  //@param idx: the palete color index:
  //@return: the RGB triple.
  uint16_t read_u16(uint32_t idx);

  //Read the RGB value of a palette entry
  //@param idx: the palete color index:
  //@return: the RGB triple as a uint16_t.
  Vera_rgb_t read_rgb(uint32_t idx);

  //Restore the default palette.
  void restore_default();

private:
  uint16_t pal_[256];

  Vera_palette();

  friend class Vera;
};

class Vera_layer {
public:
  // Retrieve the layer id.
  // @return: VERA_L0/L1
  inline Vera_layer_t id() {return layer_;}

  // Enable/disable Layer
  // @param enable: Set to true to enable Layer.
  void enable(bool enable);

  // Check if Layer is enabled
  // @return: true if Layer is enabled.
  bool enabled();

  // Set Layer horizontal scroll offset. Increase/decrease to move picture left/right.
  // @param offset: horizontal scroll offset.
  // Range: 0..255 in Bitmap mode, 0..4095 in Tile mode.
  void hscroll_set(uint32_t offset);

  // Get Layer horizontal scroll offset.
  // @return: horizontal scroll offset.
  // Range: 0..255 in Bitmap mode, 0..4095 in Tile mode.
  uint32_t hscroll_get();

  // Set Layer vertical scroll offset. Increase/decrease to move picture up/down.
  // @param offset: vertical scroll offset. Range: 0..4095.
  void vscroll_set(uint32_t offset);

  // Get Layer vertical scroll offset.
  // @return: vertical scroll offset. Range: 0..4095.
  uint32_t vscroll_get();

  // Set the Layer map width, height and map type to that of the given map.
  // @param map: Pointer to map object.
  void map_props_set(VERA_IN Vera_map *map);

  // Get the Layer's map dimensions and type.
  // @output param: Pointer to width: VERA_MAP_SZ_32/64/128/256. Skipped if
  // pointer is 0.
  // @output param: Pointer to height: VERA_MAP_SZ_32/64/128/256. Skipped if
  // pointer is 0.
  // @output param: Pointer to map_type: VERA_MAP_TYPE_TXT16/TXT256/TILE. Skipped
  // if pointer is 0.
  void map_props_get(VERA_OUT Vera_map_size_t *width,
                     VERA_OUT Vera_map_size_t *height, VERA_OUT Vera_map_type_t *map_type);

  // Set the Layer map base address to the given map.
  // @param map: Pointer to map object.
  void mapbase_set(VERA_IN Vera_map *map);

  // Get the Layer map base pointer
  // @return: Pointer to map base.
  uint8_t* mapbase_get();

  // Put Layer in Tile Mode.
  // @param tileset: the tile modes's bpp, tile width and height are taken from
  // the given tileset object.
  // Note that the tileset width and heigh must be VERA_TILE_SZ_8 or 16.
  void tilemode_set(VERA_IN Vera_tileset *tileset);

  // Get Layer Tile Mode properties. Assumes Tile mode.
  // @output param bpp: pointer to the tile color depth in bpp. Skipped if
  // pointeer is 0.
  // @output param tile_width: Pointer to tile width: VERA_TILE_SZ_8/16. Skipped
  // if pointer is 0.
  // @output param tile_height: Pointer to tile height: VERA_TILE_SZ_8/16. Skipped
  // if pointer is 0.
  void tilemode_get(VERA_OUT Vera_bpp_t *bpp,
                    VERA_OUT Vera_tile_size_t *tile_width,
                    VERA_OUT Vera_tile_size_t *tile_height);

  // Set the Layer tile base address to the given tileset.
  // @param tileset: Pointer to tileset object.
  void tilebase_set(VERA_IN Vera_tileset *tileset);

  // Get the Layer tile base pointer
  // @return: Pointer to tile base.
  uint8_t* tilebase_get();

  // Put Layer in Bitmap Mode.
  // @param bitmap: bitmap mode's bpp, and bitmap width are taken from the given
  // bitmap object.
  void bitmapmode_set(VERA_IN Vera_bitmap *bitmap);

  // Get Layer in Bitmap Mode properties. Assumes Bitmap mode.
  // @output param bpp: Pointer to the color depth in bpp. Skipped if pointer is
  // 0.
  // @output param bitmap_width: Pointer to the bitmap width in pixels. Skipped if
  // pointer is 0.
  void bitmapmode_get(VERA_OUT Vera_bpp_t *bpp, VERA_OUT Vera_bitmap_width_t *bitmap_width);

  // Set the Layer bitmap base address to the given bitmap
  // @param bitmap: Pointer to bitmap object.
  void bitmapbase_set(VERA_IN Vera_bitmap *bitmap);

  // Get the Layer bitmap base pointer
  // @return: Pointer to bitmap base.
  uint8_t* bitmapbase_get();

  // Set Layer bitmap palette offset. Assumes Bitmap mode.
  // The color index of bitmap pixels is modified by the palette offset using the following logic:
  // - Color index 0 (transparent) and 16-255 are unmodified.
  // - Color index 1-15 is modified by adding 16 x palette offset.
  // @param offset: palette offset. Range 0..15.
  void bitmap_pal_offset_set(uint32_t offset);

  // Get Layer bitmap palette offset. Assumes Bitmap mode.
  // @return: palette offset. Range 0..15.
  uint32_t bitmap_pal_offset_get();

  // Retrieve the configured Layer mode.
  // @return: VERA_LAYER_MODE_TILE/BITMAP.
  Vera_layer_mode_t mode_get();

private:
  inline Vera_layer() {}

  void set_id_(Vera_layer_t layer); //This method serves as delayed constructor.

  Vera_layer_t layer_;
  volatile Vera_layer_regs_t *vera_layer_regs_;

  friend class Vera;
};

class Vera {
public:
  Vera();

  //
  // Map Functions:
  //

  // Create a map object of the given dimensions. Store dimensions and map base pointer
  // into the object.
  // @param width: width in tiles: VERA_MAP_SZ_32/64/128/256.
  // @param height: height in tiles: VERA_MAP_SZ_32/64/128/256.
  // @param map_type: VERA_MAP_TYPE_TXT16/TXT256/TILE.
  // @return: Pointer to map object, or 0 if allocation failed.
  Vera_map* map_create(Vera_map_size_t width, Vera_map_size_t height, Vera_map_type_t map_type);

  // Delete map object, releasing its VRAM resources.
  // @param map: Pointer to map object to delete.
  void map_delete(VERA_IN Vera_map *map);

  //
  // Tile Set Functions:
  //

  // Create a tileset object using given parameters. Store parameters and tileset base
  // pointer into the tileset object.
  //
  // A tileset can be used to represent both tilesets and spritesets.
  //
  // @param width: VERA_TILE_SZ_8/16 for non-sprite tiles. For sprites,
  // additionally 32 and 64 are allowed.
  // @param height: VERA_TILE_SZ_8/16 for non-sprite tiles. For sprites,
  // additionally 32 and 64 are allowed.
  // @param bpp: VERA_BPP_1/2/4/8. In case of spritesets, only VERA_BPP_4 and 8 are
  // allowed.
  // @param num_tiles: Number of tiles in the tileset.
  // @return: tileset object or 0 if allocation failed.
  Vera_tileset* tileset_create(Vera_tile_size_t width, Vera_tile_size_t height, Vera_bpp_t bpp, uint32_t num_tiles);

  // Delete tileset object, releasing its VRAM resources.
  // @param tileset: Pointer to tileset to delete.
  void tileset_delete(VERA_IN Vera_tileset *tileset);

  //
  // Bitmap Functions:
  //

  // Create a bitmap object using given parameters. Store parameters and bitmap base
  // pointer into the object.
  // @param bitmap_base: Set to 0 to have the HAL allocate memory for a bitmap in
  // VRAM. Alternatively, pass in a pointer to the start address of a bitmap in
  // VRAM. Make sure that alignment requirements are met.
  // @param width: VERA_BITMAP_WIDTH_320/640
  // @param height: in pixels.
  // @param bpp: Supported values: VERA_BPP_1/2/4/8.
  // @return: bitmap object if succesful, 0 if memory allocation failed.
  Vera_bitmap* bitmap_create(Vera_bitmap_width_t width, uint32_t height, Vera_bpp_t bpp);

  // Delete the bitmap objecting, releaing its VRAM resources.
  // @param bitmap: Pointer to bitmap to delete.
  void bitmap_delete(VERA_IN Vera_bitmap *bitmap);

  //
  // Sprite Functions:
  //

  // Get a reference to a sprite object
  // @param id: the sprite ID. Range 0..VERA_NUM_SPRITES-1
  // @return: reference to the requested sprited object.
  Vera_sprite& sprite_get(uint32_t id);

  // Set the active sprite bank
  // @param bank: Sprite bank selector: VERA_SPR_BANK_0/1.
  inline void sprite_bank_set(Vera_sprite_bank_t bank) { VERA->CTRL = bank; }

  // Get the active sprite bank
  // @return the active sprite bank.
  inline Vera_sprite_bank_t sprite_bank_get() { return (Vera_sprite_bank_t)(VERA->CTRL_bf.SBNK); }

  //
  // Interrupt stuffs
  //

  // Enable IRQs. The passed in mask will be OR'd with the installed mask.
  // @param irq_mask: bitwise OR of VERA_IRQs to enable.
  static inline void irqs_enable(uint32_t irq_mask) { VERA->IEN |= irq_mask; }

  // Disable IRQs. The passed in mask will be inverted and  AND'd with the installed mask.
  // @param irq_mask: bitwise OR of VERA_IRQs to disable.
  static inline void irqs_disable(uint32_t irq_mask) { VERA->IEN &= ~irq_mask; }

  // Retrieve the enabled IRQs bitmask.
  // @return: a bitmask of enabled VERA_IRQs.
  static inline uint32_t irqs_enabled() { return VERA->IEN; }

  // Retrieve the active IRQs.
  // @return: a bitmask of active VERA_IRQs.
  static inline uint32_t irqs_get() { return VERA->ISR; }

  // Acknowledge IRQs.
  // @param irq_mask: bitwise OR of VERA_IRQs to acknowledge.
  static inline void irqs_ack(uint32_t irq_mask) {
    irq_mask &= VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL;
    VERA->ISR = irq_mask;
  }

  // Set the scanline on which to trigger the line IRQ if VERA_IRQ_LINE is
  // enabled.
  // @param scanline: scanline number on which the trigger the line IRQ, must be <= VERA_IRQ_LINE_MAX.
  static void irqline_set(uint32_t scanline);

  // Retrieve the scanline on which the line IRQ will be triggered if VERA_IRQ_LINE is enabled.
  // @return: line IRQ scanline number, in range 0..VERA_IRQ_LINE_MAX.
  static inline uint32_t irqline_get() { return VERA->IRQLINE; }

  //
  // General Config
  //

  // Retrieve the current scanline.
  // @return: the current scanline number, in range 0..VERA_IRQ_LINE_MAX.
  static inline uint32_t scanline_get() { return VERA->SCANLINE; }

  // Enable/disable the display.
  // @param enable: Set to true to enable the display.
  static void display_enable(bool enable);

  // Check if display is enabled
  // @return: true if display is enabled.
  static inline bool display_enabled() { return VERA->DC_VIDEO_bf.OUTPUT_MODE == VERA_DC_VIDEO_OUTPUT_MODE_VGA; }

  // Enable/disable sprite rendering.
  // @param enable: Set to true to enable sprite rendering.
  static inline void sprites_enable(bool enable) { VERA->DC_VIDEO_bf.SPR_ENABLE = enable; }

  // Check if sprite rendering is enabled
  // @return: true if sprite rendering is enabled.
  static inline bool sprites_enabled() { return VERA->DC_VIDEO_bf.SPR_ENABLE; }

  // Set the horizontal fractional scale factor.
  // @param scalefactor: unsigned 1.7 fixed point value. 1.0 means 1 output pixel for 1 input pixel. 0.5 means
  // 2 output pixels for 1 input pixel (zoom in).
  static inline void hscale_set(vera_ufix_1_7_t scalefactor) { VERA->DC_HSCALE = (uint32_t)scalefactor; }

  // Retrieve the horizontal fractional scale factor.
  // @return the scalefactor as an unsigned 1.7 fixed point value.
  static inline vera_ufix_1_7_t hscale_get() { return VERA->DC_HSCALE; }

  // Set the vertical fractional scale factor.
  // @param scalefactor: unsigned 1.7 fixed point value. 1.0 means 1 output pixel for 1 input pixel. 0.5 means
  // 2 output pixels for 1 input pixel (zoom in).
  static inline void vscale_set(vera_ufix_1_7_t scalefactor) { VERA->DC_VSCALE = (uint32_t)scalefactor; }

  // Retrieve the vertical fractional scale factor.
  // @return the scalefactor as an unsigned 1.7 fixed point value.
  static inline vera_ufix_1_7_t vscale_get() { return VERA->DC_VSCALE; }

  // Set the border color
  // @param palletteIndex: Color palette index of border color.
  static inline void bordercolor_set(uint8_t paletteIndex) { VERA->DC_BORDER = paletteIndex; }

  // Get the border color
  // @return: Color palette index of the border color.
  static inline uint8_t bordercolor_get() { return VERA->DC_BORDER; }

  // Set the boundaries of the active screen with the 640x480 space.
  // @param boundaries: pointer to screen boundaries structure.
  static void screen_boundaries_set(VERA_IN Vera_screen_boundaries_t *boundaries);

  // Retrieve the boundaries of the active screen with the 640x480 space.
  // @output param boundaries: Pointer to screen boundaries structure.
  static void screen_boundaries_get(VERA_OUT Vera_screen_boundaries_t *boundaries);

  //
  // Lower-Level VRAM access functions:
  //

  // VERA VRAM word write
  // @param addr: offset in VRAM to write word to.
  // @param data: 32-bit word to write.
  static inline void vram_wr_word(uint32_t addr, uint32_t data) {
    *(volatile uint32_t *)(addr+VERA_VRAM_BASE) = data;
  }

  // VERA VRAM byte write
  // @param addr: offset in VRAM to write byte to.
  // @param data: byte to write.
  static inline void vram_wr_byte(uint32_t addr, uint8_t data) {
    *(volatile uint8_t *)(addr+VERA_VRAM_BASE) = data;
  }

  // VERA VRAM word read
  // @param addr: offset in VRAM of word to read.
  static inline uint32_t vram_rd_word(uint32_t addr) {
    return (*(volatile uint32_t *)(addr+VERA_VRAM_BASE));
  }

  // VERA VRAM byte read
  // @param addr: offset in VRAM of byte to read.
  static inline uint8_t vram_rd_byte(uint32_t addr) {
    return (*(volatile uint8_t *)(addr+VERA_VRAM_BASE));
  }

  //
  // Lower-Level VRAM allocator functions:
  //

  // Allocate memory in VRAM for a tilemap, tiledata, bitmap or sprites.
  // The 'create' functions above use this function to allocate their resources.
  // @param size: the number of bytes to allocate (use one the vera_compute* functions
  // above).
  // @return: If successful: 2KB-aligned Pointer to allocated block of memory in VRAM. In not successful a
  // null pointer is returned.
  uint8_t* vram_alloc(uint32_t size);

  // Release allocated VRAM memory.
  // The 'delete' functions above use this function to release the resources they
  // allocated.
  // @param mem: Pointer to memory block to free.
  void vram_free(uint8_t* mem);

  //
  // The Layers
  //
  Vera_layer layer[VERA_NUM_LAYERS];

  //
  // The palette
  //
  Vera_palette palette;

private:
  static uint32_t alloc_(uint8_t *pool, uint32_t pool_size, uint32_t num_blocks);
  static void free_(uint8_t *pool, uint32_t pool_size, uint32_t idx);
  uint32_t block_id_from_ptr_(void *ptr);

  uint8_t vram_blocks_[VRAM_NUM_BLOCKS];
  uint8_t map_tileset_bitmap_blocks_[VERA_NUM_MAPS_TILESETS_BITMAPS];
  Vera_sprite sprites_[VERA_NUM_SPRITES];
  Vera_map_tileset_bitmap_u maps_tilesets_bitmaps_[VERA_NUM_MAPS_TILESETS_BITMAPS];
};

extern Vera vera;

#endif //VERA_HAL_H
