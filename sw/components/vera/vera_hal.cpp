#include "vera_hal.h"
#include "memmap.h"
#include <assert.h>
#include <string.h>

#define VRAM_BLOCK_SZ_BYTES 2048
#define VRAM_NUM_BLOCKS (VERA_VRAM_SIZE_BYTES/VRAM_BLOCK_SZ_BYTES)

typedef struct {
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
} Vera_layer_regs_t;

volatile Vera_layer_regs_t *vera_layer_regs[VERA_NUM_LAYERS];
uint8_t vram_blocks[VRAM_NUM_BLOCKS];
uint8_t sprite_blocks[VERA_NUM_SPRITE_BANKS][VERA_NUM_SPRITES_IN_BANK];

static const Vera_bpp_t bpp_dec[4] = {VERA_BPP_1,VERA_BPP_2,VERA_BPP_4,VERA_BPP_8};
static const Vera_map_size_t map_size_dec[4] = {VERA_MAP_SZ_32, VERA_MAP_SZ_64, VERA_MAP_SZ_128, VERA_MAP_SZ_256};

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

uint32_t static tile_sz_enc(Vera_tile_size_t sz) {
  uint32_t res;

  switch (sz) {
    case VERA_TILE_SZ_8: res=0; break;
    case VERA_TILE_SZ_16: res=1; break;
    case VERA_TILE_SZ_32: res=2; break;
    case VERA_TILE_SZ_64: res=3; break;
    default: assert(0);
  }

  return res;
}

void vera_hal_init() {
  vera_layer_regs[VERA_L0] = (Vera_layer_regs_t*)&(VERA->L0_CONFIG);
  vera_layer_regs[VERA_L1] = (Vera_layer_regs_t*)&(VERA->L1_CONFIG);
  memset(vram_blocks, 0, sizeof(vram_blocks));
  memset(sprite_blocks, 0, sizeof(sprite_blocks));
}

void vera_irqs_enable(uint32_t irq_mask) {
  VERA->IEN |= irq_mask;
}

void vera_irqs_disable(uint32_t irq_mask) {
  VERA->IEN &= ~irq_mask;
}

uint32_t vera_irqs_enabled() {
  return VERA->IEN;
}

void vera_irqs_ack(uint32_t irq_mask) {
  irq_mask &= VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL;
  VERA->ISR = irq_mask;
}

void vera_irqline_set(uint32_t scanline) {
  assert(scanline <= VERA_SCANLINE_MAX);
  VERA->IRQLINE = scanline;
}

uint32_t vera_irqline_get() {
  return VERA->IRQLINE;
}

uint32_t vera_scanline_get() {
  return VERA->SCANLINE;
}

void vera_display_enable(bool enable) {
  VERA->DC_VIDEO_bf.OUTPUT_MODE = enable ? VERA_DC_VIDEO_OUTPUT_MODE_VGA : VERA_DC_VIDEO_OUTPUT_MODE_DIS;
}

bool vera_display_enabled() {
  return VERA->DC_VIDEO_bf.OUTPUT_MODE == VERA_DC_VIDEO_OUTPUT_MODE_VGA;
}

void vera_sprites_enable(bool enable) {
  VERA->DC_VIDEO_bf.SPR_ENABLE = enable;
}

bool vera_sprites_enabled() {
  return VERA->DC_VIDEO_bf.SPR_ENABLE;
}

void vera_hscale_set(vera_ufix_1_7_t scalefactor) {
  VERA->DC_HSCALE = (uint32_t)scalefactor;
}

vera_ufix_1_7_t vera_hscale_get() {
  return VERA->DC_HSCALE;
}

void vera_vscale_set(vera_ufix_1_7_t scalefactor) {
  VERA->DC_VSCALE = (uint32_t)scalefactor;
}

vera_ufix_1_7_t vera_vscale_get() {
  return VERA->DC_VSCALE;
}

void vera_bordercolor_set(uint8_t paletteIndex) {
  VERA->DC_BORDER = paletteIndex;
}

uint8_t vera_bordercolor_get() {
  return VERA->DC_BORDER;
}

void vera_screen_boundaries_set(VERA_IN Vera_screen_boundaries_t *boundaries) {
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

void vera_screen_boundaries_get(VERA_OUT Vera_screen_boundaries_t *boundaries) {
  assert(boundaries);

  boundaries->hstart = VERA->DC_HSTART ;
  boundaries->hstop = VERA->DC_HSTOP;
  boundaries->vstart = VERA->DC_VSTART;
  boundaries->vstop = VERA->DC_VSTOP;
}

bool vera_map_create(uint8_t *map_base, Vera_map_size_t width, Vera_map_size_t height, Vera_map_type_t map_type, VERA_OUT Vera_map_t *map) {
  assert(map);
  map->width = width;
  map->height = height;
  map->map_type = map_type;

  map->map_base = map_base ? map_base : vera_vram_alloc(vera_compute_map_size(map));

  return map->map_base != 0;
}

void vera_map_delete(VERA_IN Vera_map_t *map) {
  assert(map);

  vera_vram_free(map->map_base);
}

uint32_t vera_compute_map_size(VERA_IN Vera_map_t *map) {
  assert(map);

  //Each entry is 2 bytes in size.
  return map->width*map->height*2;
}

void vera_map_write(VERA_IN Vera_map_t *map, uint32_t col, uint32_t row, Vera_tilemap_entry_u entry) {
  assert(map);

  uint16_t *entryp = (uint16_t*)map->map_base + row*map->width + col;
  *entryp = entry.UINT16;
}

Vera_tilemap_entry_u vera_map_read(VERA_IN Vera_map_t *map, uint32_t col, uint32_t row) {
  assert(map);
  Vera_tilemap_entry_u res;
  uint16_t *entryp = (uint16_t*)map->map_base + row*map->width + col;
  res.UINT16 = *entryp;
  return res;
}

bool vera_tileset_create(uint8_t *tileset_base, Vera_tile_size_t width, Vera_tile_size_t height, Vera_bpp_t bpp, bool is_spriteset, uint32_t num_tiles, VERA_OUT Vera_tileset_t *tileset) {
  assert(tileset);
  assert((!is_spriteset) || (bpp == VERA_BPP_4) || (bpp == VERA_BPP_8));
  assert((is_spriteset) || (width == VERA_TILE_SZ_32) || (width == VERA_TILE_SZ_64));
  assert((is_spriteset) || (height == VERA_TILE_SZ_32) || (height == VERA_TILE_SZ_64));

  tileset->width = width;
  tileset->height = height;
  tileset->bpp = bpp;
  tileset->is_spriteset = is_spriteset;
  tileset->num_tiles = num_tiles;
  tileset->tilesize_bytes = bpp*width*height/8;
  tileset->tileset_base = tileset_base ? tileset_base : vera_vram_alloc(vera_compute_tileset_size(tileset));

  return tileset->tileset_base != 0;
}

void vera_tileset_delete(VERA_IN Vera_tileset_t *tileset) {
  assert(tileset);

  vera_vram_free(tileset->tileset_base);
}

uint32_t vera_compute_tileset_size(VERA_IN Vera_tileset_t *tileset) {
  assert(tileset);
  return tileset->num_tiles*tileset->tilesize_bytes;
}

uint8_t* vera_get_tileptr(VERA_IN Vera_tileset_t *tileset, uint32_t tile_idx) {
  assert(tileset);
  assert(tile_idx < tileset->num_tiles);
  return tileset->tileset_base + tile_idx*tileset->tilesize_bytes;
}

bool vera_bitmap_create(uint8_t *bitmap_base, Vera_bitmap_width_t width, uint32_t height, Vera_bpp_t bpp, VERA_OUT Vera_bitmap_t *bitmap) {
  assert(bitmap);
  bitmap->width = width;
  bitmap->height = height;
  bitmap->bpp = bpp;
  bitmap->bitmap_base = bitmap_base ? bitmap_base : vera_vram_alloc(vera_compute_bitmap_size(bitmap));

  return bitmap->bitmap_base != 0;
}

void vera_bitmap_delete(VERA_IN Vera_bitmap_t *bitmap) {
  assert(bitmap);

  vera_vram_free(bitmap->bitmap_base);
}

uint32_t vera_compute_bitmap_size(VERA_IN Vera_bitmap_t *bitmap) {
  assert(bitmap);
  return bitmap->width*bitmap->height*bitmap->bpp/8;
}

static uint32_t alloc_(uint8_t *pool, uint32_t pool_size, uint32_t num_blocks) {
  int block_idx=0;
  int jj;

  //Find first free block
  while (block_idx<pool_size) {
    jj=0;

    while ((jj<num_blocks) && (pool[block_idx+jj]==0)) ++jj;

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

void vera_layer_enable(Vera_layer_t layer, bool enable) {
  if (layer == VERA_L0)
    VERA->DC_VIDEO_bf.L0_ENABLE = enable;
  else
    VERA->DC_VIDEO_bf.L1_ENABLE = enable;
}

bool vera_layer_enabled(Vera_layer_t layer) {
  if (layer == VERA_L0)
    return VERA->DC_VIDEO_bf.L0_ENABLE;
  else
    return VERA->DC_VIDEO_bf.L1_ENABLE;
}

void vera_layer_hscroll_set(Vera_layer_t layer, uint32_t offset) {
  assert(offset < 4096);
  vera_layer_regs[layer]->HSCROLL = offset;
}

uint32_t vera_layer_hscroll_get(Vera_layer_t layer) {
  return vera_layer_regs[layer]->HSCROLL;
}

void vera_layer_vscroll_set(Vera_layer_t layer, uint32_t offset) {
  assert(offset < 4096);
  vera_layer_regs[layer]->VSCROLL = offset;
}

uint32_t vera_layer_vscroll_get(Vera_layer_t layer) {
  return vera_layer_regs[layer]->VSCROLL;
}

void vera_layer_map_props_set(Vera_layer_t layer, VERA_IN Vera_map_t *map) {
  assert(map);
  vera_layer_regs[layer]->CONFIG_bf.T256C = (uint32_t)(map->map_type == VERA_MAP_TYPE_TXT256);
  vera_layer_regs[layer]->CONFIG_bf.MAP_WIDTH = map_size_enc(map->width);
  vera_layer_regs[layer]->CONFIG_bf.MAP_HEIGHT = map_size_enc(map->height);
}

void vera_layer_map_props_get(Vera_layer_t layer, VERA_OUT Vera_map_size_t *width,
                              VERA_OUT Vera_map_size_t *height, VERA_OUT Vera_map_type_t *map_type) {

  if (width)
    *width = map_size_dec[vera_layer_regs[layer]->CONFIG_bf.MAP_WIDTH];

  if (height)
  *height = map_size_dec[vera_layer_regs[layer]->CONFIG_bf.MAP_HEIGHT];

  if (map_type)
    if (vera_layer_regs[layer]->CONFIG_bf.COLOR_DEPTH==0)
      *map_type = vera_layer_regs[layer]->CONFIG_bf.T256C ? VERA_MAP_TYPE_TXT256 : VERA_MAP_TYPE_TXT16;
    else
      *map_type = VERA_MAP_TYPE_TILE;
}

void vera_layer_mapbase_set(Vera_layer_t layer, VERA_IN Vera_map_t *map) {
  assert(map);
  //bits16:9
  vera_layer_regs[layer]->MAPBASE = ((uint32_t)map->map_base - VERA_VRAM_BASE)>>9;
}

uint8_t* vera_layer_mapbase_get(Vera_layer_t layer) {
  //mapbase register has bits16:9 of map address.
  return (uint8_t*)VERA_VRAM_BASE + (vera_layer_regs[layer]->MAPBASE<<9);
}

void vera_layer_tilemode_set(Vera_layer_t layer,
                             VERA_IN Vera_tileset_t *tileset) {
  assert(tileset);
  vera_layer_regs[layer]->CONFIG_bf.COLOR_DEPTH = color_depth_enc(tileset->bpp);
  vera_layer_regs[layer]->CONFIG_bf.BITMAP_MODE = 0;
  vera_layer_regs[layer]->TILEBASE_bf.TILE_BITMAP_WIDTH = (uint32_t)(tileset->width == VERA_TILE_SZ_16);
  vera_layer_regs[layer]->TILEBASE_bf.TILE_HEIGHT = (uint32_t)(tileset->height == VERA_TILE_SZ_16);
}

void vera_layer_tilemode_get(Vera_layer_t layer,
                           VERA_OUT Vera_bpp_t *bpp,
                           VERA_OUT Vera_tile_size_t *tile_width,
                           VERA_OUT Vera_tile_size_t *tile_height) {

  if (bpp)
    *bpp = bpp_dec[vera_layer_regs[layer]->CONFIG_bf.COLOR_DEPTH];

  if (tile_width)
    *tile_width = vera_layer_regs[layer]->TILEBASE_bf.TILE_BITMAP_WIDTH ? VERA_TILE_SZ_16 : VERA_TILE_SZ_8;

  if (tile_height)
    *tile_height = vera_layer_regs[layer]->TILEBASE_bf.TILE_HEIGHT ? VERA_TILE_SZ_16 : VERA_TILE_SZ_8;
}

void vera_layer_tilebase_set(Vera_layer_t layer, VERA_IN Vera_tileset_t *tileset) {
  assert(tileset);

  vera_layer_regs[layer]->TILEBASE_bf.TILE_BASE_ADDR_16_11 = ((uint32_t)tileset->tileset_base - VERA_VRAM_BASE)>>11;
}

uint8_t* vera_layer_tilebase_get(Vera_layer_t layer) {
  return (uint8_t*)VERA_VRAM_BASE + (vera_layer_regs[layer]->TILEBASE_bf.TILE_BASE_ADDR_16_11<<11);
}

void vera_layer_bitmapmode_set(Vera_layer_t layer,
                               VERA_IN Vera_bitmap_t *bitmap) {
  assert(bitmap);
  vera_layer_regs[layer]->CONFIG_bf.COLOR_DEPTH = color_depth_enc(bitmap->bpp);
  vera_layer_regs[layer]->CONFIG_bf.BITMAP_MODE = 1;
  vera_layer_regs[layer]->TILEBASE_bf.TILE_BITMAP_WIDTH = (uint32_t)(bitmap->width == VERA_BITMAP_WIDTH_640);
  vera_layer_regs[layer]->TILEBASE_bf.TILE_HEIGHT = 0;
}

void vera_layer_bitmapmode_get(Vera_layer_t layer,
                               VERA_OUT Vera_bpp_t *bpp, VERA_OUT Vera_bitmap_width_t *bitmap_width) {
  if (bpp)
    *bpp = bpp_dec[vera_layer_regs[layer]->CONFIG_bf.COLOR_DEPTH];

  if (bitmap_width)
    *bitmap_width =vera_layer_regs[layer]->TILEBASE_bf.TILE_BITMAP_WIDTH ? VERA_BITMAP_WIDTH_640 : VERA_BITMAP_WIDTH_320;
}

void vera_layer_bitmapbase_set(Vera_layer_t layer, VERA_IN Vera_bitmap_t *bitmap) {
  assert(bitmap);

  vera_layer_regs[layer]->TILEBASE_bf.TILE_BASE_ADDR_16_11 = ((uint32_t)bitmap->bitmap_base - VERA_VRAM_BASE)>>11;
}

uint8_t* vera_layer_bitmapbase_get(Vera_layer_t layer) {
  return (uint8_t*)VERA_VRAM_BASE + (vera_layer_regs[layer]->TILEBASE_bf.TILE_BASE_ADDR_16_11<<11);
}

void vera_layer_bitmap_pal_offset_set(Vera_layer_t layer, uint32_t offset) {
  assert(offset < 16);
  vera_layer_regs[layer]->HSCROLL_bf.HSCROLL_11_8_PAL_OFFSET = offset;
}

uint32_t vera_layer_bitmap_pal_offset_get(Vera_layer_t layer) {
  return vera_layer_regs[layer]->HSCROLL_bf.HSCROLL_11_8_PAL_OFFSET;
}

Vera_layer_mode_t vera_layer_mode_get(Vera_layer_t layer) {
  return vera_layer_regs[layer]->CONFIG_bf.BITMAP_MODE ? VERA_LAYER_MODE_BITMAP : VERA_LAYER_MODE_TILE;
}

void vera_palette_wr(uint32_t idx, Vera_rgb_u rgb) {
  //One color entry per word address
  *(uint32_t *)(idx*4+VERA_PALETTE_RAM_BASE) = (uint32_t)rgb.UINT16;
}

//Given a block pool of a given size, free the allocated block starting with
//idx;
static void free_(uint8_t *pool, uint32_t pool_size, uint32_t idx) {

  assert(idx < pool_size);

  uint8_t num_blocks = pool[idx];

  //If num_blocks is 0xFF, idx is not pointing to the beginning of a block.
  assert(num_blocks != 0xFF);

  //Set all entries to 0, marking them as free.
  memset(pool + idx, 0, (size_t)num_blocks);
}

uint8_t* vera_vram_alloc(uint32_t size) {
  //Convert size in bytes to block size, rounding up.
  uint32_t num_blocks = (size+VRAM_BLOCK_SZ_BYTES-1)/VRAM_BLOCK_SZ_BYTES;
  uint32_t block_id = alloc_(vram_blocks, VRAM_NUM_BLOCKS, size);
  if (block_id == ~0U)
    return 0;

  return (uint8_t*)(VERA_VRAM_BASE+block_id*VRAM_BLOCK_SZ_BYTES);
}

void vera_vram_free(uint8_t* mem) {
  assert(mem);
  uint32_t block_id = ((uint8_t*)(VERA_VRAM_BASE)-mem)*VRAM_BLOCK_SZ_BYTES;

  assert(block_id < VRAM_NUM_BLOCKS);

  free_(vram_blocks, VRAM_NUM_BLOCKS, block_id);
}

uint32_t vera_spr_ids_alloc(Vera_sprite_bank_t bank, uint32_t num_sprs) {
  return alloc_(sprite_blocks[bank], VERA_NUM_SPRITES_IN_BANK, num_sprs);
}

void vera_spr_ids_free(uint32_t sprite_id) {
  assert(sprite_id < VERA_NUM_SPRITES_IN_BANK*VERA_NUM_SPRITE_BANKS);
  Vera_sprite_bank_t bank = (sprite_id > VERA_NUM_SPRITES_IN_BANK) ? VERA_SPR_BANK_1 : VERA_SPR_BANK_0;
  free_(sprite_blocks[bank], VERA_NUM_SPRITES_IN_BANK, sprite_id);
}

void vera_spr_attr_set(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite) {
  assert(sprite);
  assert((sprite->bpp == VERA_BPP_4) || (sprite->bpp == VERA_BPP_8));
  assert(sprite->collision_mask < 15);

  uint32_t vv,ww;

  vv = (uint32_t)(sprite->addr-VERA_VRAM_BASE)>>5;
  vv |= ((uint32_t)(sprite->bpp == VERA_BPP_8))<<15;
  vv |= ((uint32_t)sprite->x)<<16;
  ww = (uint32_t)sprite->y;
  ww |= (sprite->z_depth<<18);
  ww |= (sprite->collision_mask<<20);
  ww |= tile_sz_enc(sprite->width)<<28;
  ww |= tile_sz_enc(sprite->height)<<30;

  *(uint32_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8) = vv;
  *(uint32_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8 + 4) = ww;
}

void vera_spr_attr_update_addr(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite) {
  assert(sprite);
  uint16_t v;

  v = (uint16_t)((uint32_t)sprite->addr-VERA_VRAM_BASE)>>5;
  v |= ((uint16_t)(sprite->bpp == VERA_BPP_8))<<15;

  *(uint16_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8) = v;
}

void vera_spr_attr_update_x(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite) {
  assert(sprite);

  *(volatile uint16_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8 + 2) = sprite->x;
}

void vera_spr_attr_update_y(uint32_t sprite_id, VERA_IN Vera_sprite_attrs_t *sprite) {
  assert(sprite);

  *(uint16_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8 + 4) = sprite->y;
}

