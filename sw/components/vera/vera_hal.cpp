#include "vera_hal.h"
#include "memmap.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

//A 16-bit tile map entry, an overlay of any of the three prior structs.
typedef union {
  uint16_t UINT16;
  Vera_textmap_entry_16_t txt16;
  Vera_textmap_entry_256_t txt256;
  Vera_tilemap_entry_t tile;
} Vera_map_entry_u;

struct Vera_layer_regs_t {
    union {
        uint32_t CONFIG; // Layer Configuration regiser.
        vera_l0_config_t CONFIG_bf; // Bit access for CONFIG register
    };
    union {
        uint32_t MAPBASE; // Layer map base register.
        vera_l0_mapbase_t MAPBASE_bf; // Bit access for MAPBASE register
    };
    union {
        uint32_t TILEBASE; // Layer tile base register.
        vera_l0_tilebase_t TILEBASE_bf; // Bit access for TILEBASE register
    };
    uint32_t RESERVED2[1];
    union {
        uint32_t HSCROLL; // Layer horizontal scroll register.
        vera_l0_hscroll_t HSCROLL_bf; // Bit access for HSCROLL register
    };
    union {
        uint32_t VSCROLL; // Layer vertical scroll register.
        vera_l0_vscroll_t VSCROLL_bf; // Bit access for VSCROLL register
    };
};

static const Vera_bpp_t bpp_dec[4] = {VERA_BPP_1,VERA_BPP_2,VERA_BPP_4,VERA_BPP_8};
static const Vera_map_size_t map_size_dec[4] = {VERA_MAP_SZ_32, VERA_MAP_SZ_64, VERA_MAP_SZ_128, VERA_MAP_SZ_256};
static const uint16_t default_palette[256] = {
  0x000,0xfff,0x800,0xafe,0xc4c,0x0c5,0x00a,0xee7,0xd85,0x640,0xf77,0x333,0x777,0xaf6,0x08f,0xbbb,
  0x000,0x111,0x222,0x333,0x444,0x555,0x666,0x777,0x888,0x999,0xaaa,0xbbb,0xccc,0xddd,0xeee,0xfff,
  0x211,0x433,0x644,0x866,0xa88,0xc99,0xfbb,0x211,0x422,0x633,0x844,0xa55,0xc66,0xf77,0x200,0x411,
  0x611,0x822,0xa22,0xc33,0xf33,0x200,0x400,0x600,0x800,0xa00,0xc00,0xf00,0x221,0x443,0x664,0x886,
  0xaa8,0xcc9,0xfeb,0x211,0x432,0x653,0x874,0xa95,0xcb6,0xfd7,0x210,0x431,0x651,0x862,0xa82,0xca3,
  0xfc3,0x210,0x430,0x640,0x860,0xa80,0xc90,0xfb0,0x121,0x343,0x564,0x786,0x9a8,0xbc9,0xdfb,0x121,
  0x342,0x463,0x684,0x8a5,0x9c6,0xbf7,0x120,0x241,0x461,0x582,0x6a2,0x8c3,0x9f3,0x120,0x240,0x360,
  0x480,0x5a0,0x6c0,0x7f0,0x121,0x343,0x465,0x686,0x8a8,0x9ca,0xbfc,0x121,0x242,0x364,0x485,0x5a6,
  0x6c8,0x7f9,0x020,0x141,0x162,0x283,0x2a4,0x3c5,0x3f6,0x020,0x041,0x061,0x082,0x0a2,0x0c3,0x0f3,
  0x122,0x344,0x466,0x688,0x8aa,0x9cc,0xbff,0x122,0x244,0x366,0x488,0x5aa,0x6cc,0x7ff,0x022,0x144,
  0x166,0x288,0x2aa,0x3cc,0x3ff,0x022,0x044,0x066,0x088,0x0aa,0x0cc,0x0ff,0x112,0x334,0x456,0x668,
  0x88a,0x9ac,0xbcf,0x112,0x224,0x346,0x458,0x56a,0x68c,0x79f,0x002,0x114,0x126,0x238,0x24a,0x35c,
  0x36f,0x002,0x014,0x016,0x028,0x02a,0x03c,0x03f,0x112,0x334,0x546,0x768,0x98a,0xb9c,0xdbf,0x112,
  0x324,0x436,0x648,0x85a,0x96c,0xb7f,0x102,0x214,0x416,0x528,0x62a,0x83c,0x93f,0x102,0x204,0x306,
  0x408,0x50a,0x60c,0x70f,0x212,0x434,0x646,0x868,0xa8a,0xc9c,0xfbe,0x211,0x423,0x635,0x847,0xa59,
  0xc6b,0xf7d,0x201,0x413,0x615,0x826,0xa28,0xc3a,0xf3c,0x201,0x403,0x604,0x806,0xa08,0xc09,0xf0b};

Vera vera;

static void pixel_set_8bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width, uint8_t val) {
  base[y*width + x] = val;
}

static uint32_t pixel_get_8bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width) {
  return base[y*width + x];
}

static void pixel_set_4bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width, uint8_t val) {
  uint8_t *p = base + (y*width + x)/2;
  uint8_t b = *p;

  //Left or right half of the byte
  if (x&1) {
    b &= 0xF0;
    b |= val&0xF;
  }
  else {
    b &= 0xF;
    b |= val<<4;
  }

  *p = b;
}

static uint8_t pixel_get_4bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width) {
  uint8_t *p = base + (y*width + x)/2;
  uint8_t b = *p;

  //Left or right half of the byte
  if (x&1)
    return b&0xF;
  else
    return b>>4;
}

static void pixel_set_2bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width, uint8_t val) {
  uint8_t *p = base + (y*width + x)/4;
  uint8_t b = *p;
  static const uint32_t bitoffsets[] = {6, 4, 2, 0};
  uint32_t bitoffset = bitoffsets[x&2];

  b &= ~(3<<bitoffset);
  b |= (val&3)<<bitoffset;

  *p = b;
}

static uint8_t pixel_get_2bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width) {
  uint8_t *p = base + (y*width + x)/4;
  uint8_t b = *p;
  static const uint32_t bitoffsets[] = {6, 4, 2, 0};
  uint32_t bitoffset = bitoffsets[x&2];

  b >>= bitoffset;

  return b&0x3;
}

static void pixel_set_1bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width, uint8_t val) {
  uint8_t *p = base + (y*width + x)/8;
  uint8_t b = *p;
  uint32_t bitoffset = 7-(x&3);

  b &= ~(1<<bitoffset);
  b |= (val&1)<<bitoffset;

  *p = b;
}

static uint8_t pixel_get_1bpp(uint8_t *base, uint32_t x, uint32_t y, uint32_t width) {
  uint8_t *p = base + (y*width + x)/8;
  uint8_t b = *p;
  uint32_t bitoffset = 7-(x&3);

  b >>= bitoffset;
  return b&1;
}

static void pixel_set(Vera_bpp_t bpp, uint8_t *base, uint32_t x, uint32_t y, uint32_t width, uint8_t val) {
  switch (bpp) {
    case VERA_BPP_1:
      pixel_set_1bpp(base, x, y, width, val);
      return;
    case VERA_BPP_2:
      pixel_set_2bpp(base, x, y, width, val);
      return;
    case VERA_BPP_4:
      pixel_set_4bpp(base, x, y, width, val);
      return;
    case VERA_BPP_8:
      pixel_set_8bpp(base, x, y, width, val);
      return;
    default:
      assert(false);
  }
}

static uint8_t pixel_get(Vera_bpp_t bpp, uint8_t *base, uint32_t x, uint32_t y, uint32_t width) {
  switch (bpp) {
    case VERA_BPP_1:
      return pixel_get_1bpp(base, x, y, width);
    case VERA_BPP_2:
      return pixel_get_2bpp(base, x, y, width);
    case VERA_BPP_4:
      return pixel_get_4bpp(base, x, y, width);
    case VERA_BPP_8:
      return pixel_get_8bpp(base, x, y, width);
    default:
      assert(false);
  }

  return 0;
}

uint32_t static map_size_enc(Vera_map_size_t sz) {
  uint32_t res;

  switch (sz) {
    case VERA_MAP_SZ_32: res=0; break;
    case VERA_MAP_SZ_64: res=1; break;
    case VERA_MAP_SZ_128: res=2; break;
    case VERA_MAP_SZ_256: res=3; break;
    default: assert(0);
  }

  return res;
}

uint32_t static color_depth_enc(Vera_bpp_t bpp) {
  uint32_t res;

  switch (bpp) {
    case VERA_BPP_1: res=0; break;
    case VERA_BPP_2: res=1; break;
    case VERA_BPP_4: res=2; break;
    case VERA_BPP_8: res=3; break;
    default: assert(0);
  }

  return res;
}

uint16_t static tile_sz_enc(Vera_tile_size_t sz) {
  uint16_t res;

  switch (sz) {
    case VERA_TILE_SZ_8: res=0; break;
    case VERA_TILE_SZ_16: res=1; break;
    case VERA_TILE_SZ_32: res=2; break;
    case VERA_TILE_SZ_64: res=3; break;
    default: assert(0);
  }

  return res;
}

bool Vera_map::init(Vera_map_size_t width, Vera_map_size_t height, Vera_map_type_t map_type) {
  assert(!initialized_);

  map_base_ = vera.vram_alloc(width*height*2);
  if (!map_base_) return false;

  width_ = width;
  height_ = height;
  map_type_ = map_type;
  initialized_ = true;

  return true;
}

void Vera_map::deinit() {
  assert(initialized_);

  vera.vram_free(map_base_);
  initialized_ = false;
}

void Vera_map::write(uint32_t col, uint32_t row, uint16_t entry) {
  assert(initialized_);

  assert(col < width_);
  assert(row < height_);

  uint16_t *entryp = (uint16_t*)map_base_ + row*width_ + col;
  *entryp = entry;
}

void Vera_map::write(uint32_t col, uint32_t row, Vera_textmap_entry_16_t entry) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TXT16);

  Vera_map_entry_u entry_u;
  entry_u.txt16 = entry;
  write(col, row, entry_u.UINT16);
}

void Vera_map::write(uint32_t col, uint32_t row, Vera_textmap_entry_256_t entry) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TXT256);

  Vera_map_entry_u entry_u;
  entry_u.txt256 = entry;
  write(col, row, entry_u.UINT16);
}

void Vera_map::write(uint32_t col, uint32_t row, Vera_tilemap_entry_t entry) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TILE);
  Vera_map_entry_u entry_u;
  entry_u.tile = entry;
  write(col, row, entry_u.UINT16);
}

uint16_t Vera_map::read(uint32_t col, uint32_t row) {
  assert(initialized_);

  uint16_t *entryp = (uint16_t*)(map_base_ + 2*(row*width_ + col));
  return *entryp;
}

Vera_textmap_entry_16_t Vera_map::read_txt16(uint32_t col, uint32_t row) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TXT16);

  Vera_map_entry_u entry_u;
  entry_u.UINT16 = read(col, row);
  return entry_u.txt16;
}

Vera_textmap_entry_256_t Vera_map::read_txt256(uint32_t col, uint32_t row) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TXT256);

  Vera_map_entry_u entry_u;
  entry_u.UINT16 = read(col, row);
  return entry_u.txt256;
}

Vera_tilemap_entry_t Vera_map::read_tile(uint32_t col, uint32_t row) {
  assert(initialized_);
  assert(map_type_ == VERA_MAP_TYPE_TILE);

  Vera_map_entry_u entry_u;
  entry_u.UINT16 = read(col, row);
  return entry_u.tile;
}

bool Vera_tileset::init(Vera_tile_size_t width, Vera_tile_size_t height, Vera_bpp_t bpp, uint32_t num_tiles) {
  assert(!initialized_);
  assert(num_tiles > 0);

  tileset_base_ = vera.vram_alloc(num_tiles*bpp*width*height/8);
  if (!tileset_base_) return false;

  width_ = width;
  height_ = height;
  bpp_ = bpp;
  num_tiles_ = num_tiles;
  tilesize_bytes_ = bpp*width*height/8;
  initialized_ = true;

  return true;
}

void Vera_tileset::deinit() {
  assert(initialized_);
  vera.vram_free(tileset_base_);
  initialized_ = false;
}

uint8_t* Vera_tileset::tile(uint32_t tile_idx) {
  assert(initialized_);
  assert(tile_idx < num_tiles_);
  return tileset_base_ + tile_idx*tilesize_bytes_;
}

void Vera_tileset::pixel_set(uint32_t tile_idx, uint32_t x, uint32_t y, uint8_t val) {
  assert(initialized_);
  assert(x < width_);
  assert(y < height_);

  uint8_t *tileptr = tile(tile_idx);

  ::pixel_set(bpp_, tileptr, x, y, width_, val);
}

uint8_t Vera_tileset::pixel_get(uint32_t tile_idx, uint32_t x, uint32_t y) {
  assert(initialized_);
  assert(x < width_);
  assert(y < height_);

  uint8_t *tileptr = tile(tile_idx);

  return ::pixel_get(bpp_, tileptr, x, y, width_);
}

bool Vera_bitmap::init(Vera_bitmap_width_t width, uint32_t height, Vera_bpp_t bpp) {
  assert(!initialized_);
  assert(height > 0);

  bitmap_base_ = vera.vram_alloc(width*height*bpp/8);
  if (!bitmap_base_) return false;

  width_ = width;
  height_ = height;
  bpp_ = bpp;
  initialized_ = true;

  return true;
}

void Vera_bitmap::deinit() {
  assert(initialized_);

  vera.vram_free(bitmap_base_);

  initialized_ = false;
}

void Vera_bitmap::pixel_set(uint32_t x, uint32_t y, uint8_t val) {
  assert(initialized_);
  assert(x < width_);
  assert(y < height_);

  ::pixel_set(bpp_, bitmap_base_, x, y, width_, val);
}

uint8_t Vera_bitmap::pixel_get(uint32_t x, uint32_t y) {
  assert(initialized_);
  assert(x < width_);
  assert(y < height_);

  return ::pixel_get(bpp_, bitmap_base_, x, y, width_);
}

void Vera_sprite::init() {
  assert(attrs_);

  tileset_id_ = ~0U;
  tile_idx_ = 0;
  x_ = 0;
  y_ = 0;
  pal_offset_ = 0;
  collision_mask_ = 0;
  z_depth_ = VERA_SPR_Z_DIS;
  vflip_ = false;
  hflip_ = false;

  attrs_[0] = 0;
  attrs_[1] = 0;
  attrs_[2] = 0;
  attrs_[3] = 0;
}

void Vera_sprite::tile_set(uint32_t tileset_id, uint32_t tile_idx) {
  assert(tileset_id < VERA_NUM_TILESETS);

  tileset_id_ = tileset_id;
  tile_idx_ = tile_idx;

  Vera_tileset& tileset = vera.tileset[tileset_id];

  uint8_t *addr = tileset.tile(tile_idx_);
  Vera_bpp_t bpp = tileset.bpp();

  uint16_t aa, ww;
  aa = (uint16_t)((uint32_t)addr-VERA_VRAM_BASE)>>5;
  aa |= ((uint16_t)(bpp == VERA_BPP_8))<<15;
  ww = (uint16_t)hflip_;
  ww |= (uint16_t)(vflip_)<<1;
  ww |= (uint16_t)(z_depth_)<<2;
  ww |= (uint16_t)(collision_mask_)<<4;
  ww |= (uint16_t)(pal_offset_)<<8;
  ww |= tile_sz_enc(tileset.width())<<12;
  ww |= tile_sz_enc(tileset.height())<<14;

  assert(attrs_);

  attrs_[0] = aa;
  attrs_[3] = ww;
}

void Vera_sprite::tile_get(VERA_OUT uint32_t &tileset_id, VERA_OUT uint32_t& tile_idx) {
  tileset_id = tileset_id_;
  tile_idx = tile_idx_;
}

void Vera_sprite::x_set(uint16_t x) {
  assert(attrs_);

  x_ = x;
  attrs_[1] = x;
}

void Vera_sprite::y_set(uint16_t y) {
  assert(attrs_);

  y_ = y;
  attrs_[2] = y;
}

void Vera_sprite::pal_offset_set(uint8_t pal_offset) {
  assert(pal_offset < 16);

  pal_offset_ = pal_offset;

  set_attr_byte7_();
}

void Vera_sprite::collision_mask_set(uint8_t collision_mask) {
  assert(collision_mask < 16);
  collision_mask_ = collision_mask;

  set_attr_byte6_();
}

void Vera_sprite::z_depth_set(Vera_z_depth_t z_depth) {
  z_depth_ = z_depth;
  set_attr_byte6_();
}

void Vera_sprite::hflip_set(bool hflip) {
  hflip_ = hflip;

  set_attr_byte6_();
}

void Vera_sprite::vflip_set(bool vflip) {
  vflip_ = vflip;

  set_attr_byte6_();
}

void Vera_sprite::pixel_set(uint32_t x, uint32_t y, uint8_t val) {
  assert(tileset_id_ < VERA_NUM_TILESETS);

  Vera_tileset& tileset = vera.tileset[tileset_id_];

  tileset.pixel_set(tile_idx_, x, y, val);
}

uint8_t Vera_sprite::pixel_get(uint32_t x, uint32_t y) {
  assert(tileset_id_ < VERA_NUM_TILESETS);

  Vera_tileset& tileset = vera.tileset[tileset_id_];

  return tileset.pixel_get(tile_idx_, x, y);
}

void Vera_sprite::set_id_(uint32_t id) {
  id_ = id;
  attrs_ = (uint16_t *)(VERA_SPRITE_RAM_BASE + id*8);
  init();
}

void Vera_sprite::set_attr_byte6_() {
  assert(attrs_);

  uint16_t bb;
  bb = (uint8_t)hflip_;
  bb |= (uint8_t)(vflip_)<<1;
  bb |= (uint8_t)(z_depth_)<<2;
  bb |= (uint8_t)(collision_mask_)<<4;

  uint8_t *attrb_ = (uint8_t*)(&attrs_[3]);
  *attrb_ = bb;
}

void Vera_sprite::set_attr_byte7_() {
  assert(attrs_);
  assert(tileset_id_ < VERA_NUM_TILESETS);

  Vera_tileset& tileset = vera.tileset[tileset_id_];

  uint8_t bb = pal_offset_;
  bb |= tile_sz_enc(tileset.width())<<4;
  bb |= tile_sz_enc(tileset.height())<<6;

  uint8_t *attrb_ = (uint8_t*)(&attrs_[3]) + 1;
  *attrb_ = bb;
}

void Vera_palette::write(uint32_t idx, Vera_rgb_t rgb) {
  Vera_rgb_u rgbu;
  rgbu.rgb = rgb;

  write(idx, rgbu.UINT16);
}

void Vera_palette::write(uint32_t idx, uint16_t val) {
  assert (idx < 256);
  val &= 0xfff;

  pal_[idx] = val;

  //One color entry per word address
  *(uint32_t *)(idx*4+VERA_PALETTE_RAM_BASE) = (uint32_t)(val);
}

uint16_t Vera_palette::read_u16(uint32_t idx) {
  assert (idx < 256);
  return pal_[idx];
}

Vera_rgb_t Vera_palette::read_rgb(uint32_t idx) {
  Vera_rgb_u rgbu;
  rgbu.UINT16 = read_u16(idx);

  return rgbu.rgb;
}

void Vera_palette::restore_default() {
  for (int ii=0; ii<256; ii++) write(ii, default_palette[ii]);
}

Vera_palette::Vera_palette() {
  restore_default();
}

void Vera_layer::enable(bool enable) {
  if (layer_ == VERA_L0)
    VERA->DC_VIDEO_bf.L0_ENABLE = enable;
  else
    VERA->DC_VIDEO_bf.L1_ENABLE = enable;
}

bool Vera_layer::enabled() {
  if (layer_ == VERA_L0)
    return VERA->DC_VIDEO_bf.L0_ENABLE;
  else
    return VERA->DC_VIDEO_bf.L1_ENABLE;
}

void Vera_layer::hscroll_set(uint32_t offset) {
  assert(offset < 4096);
  vera_layer_regs_->HSCROLL = offset;
}

uint32_t Vera_layer::hscroll_get() {
  return vera_layer_regs_->HSCROLL;
}

void Vera_layer::vscroll_set(uint32_t offset) {
  assert(offset < 4096);
  vera_layer_regs_->VSCROLL = offset;
}

uint32_t Vera_layer::vscroll_get() {
  return vera_layer_regs_->VSCROLL;
}

void Vera_layer::map_set(uint32_t map_id) {
  assert(map_id < VERA_NUM_MAPS);
  map_id_ = map_id;

  Vera_map& map = vera.map[map_id];

  vera_layer_regs_->CONFIG_bf.T256C = (uint32_t)(map.map_type() == VERA_MAP_TYPE_TXT256);
  vera_layer_regs_->CONFIG_bf.MAP_WIDTH = map_size_enc(map.width());
  vera_layer_regs_->CONFIG_bf.MAP_HEIGHT = map_size_enc(map.height());
  vera_layer_regs_->MAPBASE = ((uint32_t)map.map_base() - VERA_VRAM_BASE)>>9;
}

void Vera_layer::tileset_set(uint32_t tileset_id) {
  assert(tileset_id < VERA_NUM_TILESETS);

  Vera_tileset& tileset = vera.tileset[tileset_id];

  tileset_id_ = tileset_id;
  bitmap_id_ = ~0U;

  vera_layer_regs_->CONFIG_bf.COLOR_DEPTH = color_depth_enc(tileset.bpp());
  vera_layer_regs_->CONFIG_bf.BITMAP_MODE = 0;
  vera_layer_regs_->TILEBASE_bf.TILE_BITMAP_WIDTH = (uint32_t)(tileset.width() == VERA_TILE_SZ_16);
  vera_layer_regs_->TILEBASE_bf.TILE_HEIGHT = (uint32_t)(tileset.height() == VERA_TILE_SZ_16);
  vera_layer_regs_->TILEBASE_bf.TILE_BASE_ADDR_16_11 = ((uint32_t)tileset.tileset_base() - VERA_VRAM_BASE)>>11;
}

void Vera_layer::bitmap_set(uint32_t bitmap_id) {
  assert(bitmap_id < VERA_NUM_BITMAPS);

  tileset_id_ = ~0U;
  bitmap_id_ = bitmap_id;

  Vera_bitmap &bitmap = vera.bitmap[bitmap_id];

  vera_layer_regs_->CONFIG_bf.COLOR_DEPTH = color_depth_enc(bitmap.bpp());
  vera_layer_regs_->CONFIG_bf.BITMAP_MODE = 1;
  vera_layer_regs_->TILEBASE_bf.TILE_BITMAP_WIDTH = (uint32_t)(bitmap.width() == VERA_BITMAP_WIDTH_640);
  vera_layer_regs_->TILEBASE_bf.TILE_HEIGHT = 0;
  vera_layer_regs_->TILEBASE_bf.TILE_BASE_ADDR_16_11 = ((uint32_t)bitmap.bitmap_base() - VERA_VRAM_BASE)>>11;
}

void Vera_layer::bitmap_pal_offset_set(uint8_t offset) {
  assert(offset < 16);
  vera_layer_regs_->HSCROLL_bf.HSCROLL_11_8_PAL_OFFSET = (uint32_t)offset;
}

uint8_t Vera_layer::bitmap_pal_offset_get() {
  return (uint8_t)vera_layer_regs_->HSCROLL_bf.HSCROLL_11_8_PAL_OFFSET;
}

void Vera_layer::set_id_(Vera_layer_t layer) {
  layer_ = layer;
  vera_layer_regs_ = (Vera_layer_regs_t*)&((layer == VERA_L0) ? VERA->L0_CONFIG : VERA->L1_CONFIG);
  map_id_ = ~0U;
  tileset_id_ = ~0U;
  bitmap_id_ = ~0U;
}

Vera::Vera() {
  init();
}

void Vera::init() {
  memset(vram_blocks_, 0, sizeof(vram_blocks_));

  for (int ii=0; ii<VERA_NUM_SPRITES; ii++) {
    sprite[ii].set_id_(ii);
  }

  //Initialize the layer objects
  layer[VERA_L0].set_id_(VERA_L0);
  layer[VERA_L1].set_id_(VERA_L1);
}

Vera_rgb_t Vera::line_capture_read_pixel(uint32_t x) {
  assert(x < 640);

  Vera_rgb_u rgbu;

  rgbu.UINT16 = *(uint16_t *)(x*4+VERA_CAPTURE_RAM_BASE);

  return rgbu.rgb;
}

void Vera::irqline_set(uint32_t scanline) {
  assert(scanline <= VERA_SCANLINE_MAX);
  VERA->IRQLINE = scanline;
}

void Vera::display_enable(bool enable) {
  VERA->DC_VIDEO_bf.OUTPUT_MODE = enable ? VERA_DC_VIDEO_OUTPUT_MODE_VGA : VERA_DC_VIDEO_OUTPUT_MODE_DIS;
}

void Vera::screen_boundaries_set(VERA_IN Vera_screen_boundaries_t *boundaries) {
  assert(boundaries);
  assert (boundaries->hstart < boundaries->hstop);
  assert (boundaries->vstart < boundaries->vstop);
  assert (boundaries->hstop < VERA_HSTOP_MAX);
  assert (boundaries->vstop < VERA_VSTOP_MAX);

  VERA->DC_HSTART = boundaries->hstart;
  VERA->DC_HSTOP = boundaries->hstop;
  VERA->DC_VSTART = boundaries->vstart;
  VERA->DC_VSTOP = boundaries->vstop;
}

void Vera::screen_boundaries_get(VERA_OUT Vera_screen_boundaries_t *boundaries) {
  assert(boundaries);

  boundaries->hstart = VERA->DC_HSTART ;
  boundaries->hstop = VERA->DC_HSTOP;
  boundaries->vstart = VERA->DC_VSTART;
  boundaries->vstop = VERA->DC_VSTOP;
}

uint32_t Vera::alloc_(uint8_t *pool, uint32_t pool_size, uint32_t num_blocks) {
  int block_idx=0;
  int jj;

  //Find first free block
  while (block_idx<pool_size) {
    jj=0;

    while ((jj<num_blocks) && (block_idx+jj<pool_size) && (pool[block_idx+jj]==0)) ++jj;

    if (jj == num_blocks) break;

    block_idx += jj+1; //Update block_idx so we continue the search where we left off.
  }

  //If we found a block...
  if (block_idx < pool_size) {
    //Write the allocated size into this block entry, so we can release the
    //block later.
    pool[block_idx] = (uint8_t)num_blocks;
    //Set the remaining entries to 0xFF to mark them as allocated
    memset(pool + block_idx + 1, 0xFF, num_blocks-1);
  }
  else {
    block_idx = ~0U; //Set to ~0U to indicate allocation failure.
  }

  return block_idx;
}

//Given a block pool of a given size, free the allocated block starting with
//idx;
void Vera::free_(uint8_t *pool, uint32_t pool_size, uint32_t idx) {

  assert(idx < pool_size);

  uint8_t num_blocks = pool[idx];

  //If num_blocks is 0xFF, idx is not pointing to the beginning of a block.
  assert(num_blocks != 0xFF);

  //Set all entries to 0, marking them as free.
  memset(pool + idx, 0, (size_t)num_blocks);
}

uint8_t* Vera::vram_alloc(uint32_t size) {
  //Convert size in bytes to block size, rounding up.
  uint32_t num_blocks = (size+VRAM_BLOCK_SZ_BYTES-1)/VRAM_BLOCK_SZ_BYTES;
  uint32_t block_id = alloc_(vram_blocks_, VRAM_NUM_BLOCKS, num_blocks);
  if (block_id == ~0U)
    return 0;

  return (uint8_t*)(VERA_VRAM_BASE+block_id*VRAM_BLOCK_SZ_BYTES);
}

void Vera::vram_free(uint8_t* mem) {
  assert(mem);
  uint32_t block_id = (mem - (uint8_t*)(VERA_VRAM_BASE))/VRAM_BLOCK_SZ_BYTES;

  assert(block_id < VRAM_NUM_BLOCKS);

  free_(vram_blocks_, VRAM_NUM_BLOCKS, block_id);
}


