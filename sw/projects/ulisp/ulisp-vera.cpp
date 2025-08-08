#include "vera_hal.h"
#include "ulisp.h"

#define cdddr(x)           cdr(cdr(cdr(x)))
#define fourth(x)          first(cdddr(x))

#define cddddr(x)          cdr(cdr(cdr(cdr(x))))
#define fifth(x)           first(cddddr(x))

const char insufficientresources[] = "insufficient resources";

const char alreadyinitialized[] = "already initialized";

const char notinitialized[] = "not initialized";

Vera_bitmap_width_t checkbitmapwidth (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if (!((i==VERA_BITMAP_WIDTH_320) || (i==VERA_BITMAP_WIDTH_640)))
    error(invalidarg,obj);

  return (Vera_bitmap_width_t)i;
}

Vera_bpp_t checkbpp (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if (!((i==VERA_BPP_1) || (i==VERA_BPP_2) || (i==VERA_BPP_4) || (i==VERA_BPP_8)))
    error(invalidarg,obj);

  return (Vera_bpp_t)i;
}

Vera_tile_size_t checktilesize (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if (!((i==VERA_TILE_SZ_8) || (i==VERA_TILE_SZ_16) || (i==VERA_TILE_SZ_32) || (i==VERA_TILE_SZ_64)))
    error(invalidarg,obj);

  return (Vera_tile_size_t)i;
}

Vera_map_size_t checkmapsize (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if (!((i==VERA_MAP_SZ_32) || (i==VERA_MAP_SZ_64) || (i==VERA_MAP_SZ_128) || (i==VERA_MAP_SZ_256)))
    error(invalidarg,obj);

  return (Vera_map_size_t)i;
}

Vera_map_type_t checkmaptype (object *obj) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if ((i<VERA_MAP_TYPE_TXT16) || (i>VERA_MAP_TYPE_TILE))
    error(invalidarg,obj);

  return (Vera_map_type_t)i;
}

uint32_t checkrange (object *obj, int min, int max) {
  if (!integerp(obj)) error(notaninteger, obj);

  int i = obj->integer;

  if ((i<min) || (i>max))
    error(invalidarg,obj);

  return (uint32_t)i;
}

float checkrangefloat (object *obj, float min, float max) {
  if (!floatp(obj)) error(notanumber, obj);

  float f = obj->single_float;

  if ((f<min) || (f>max))
    error(invalidarg,obj);

  return f;
}

// Definitions
object *fn_vera_init (object *args, object *env) {
  (void) env;

  vera.init();

  return NULL;
}
object *fn_vera_map (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t map_id = checkrange(first(args), 0,  VERA_NUM_MAPS-1);

  Vera_map &map = vera.map[map_id];

  if (nargs == 4) {
    if (map.is_initialized())
      error(alreadyinitialized, first(args));

    bool res = map.init(
      (Vera_map_size_t)checkmapsize(second(args)),
      (Vera_map_size_t)checkmapsize(third(args)),
      (Vera_map_type_t)checkmaptype(fourth(args)));

    if (!res)
      error2(insufficientresources);
  }
  else if (nargs > 1) {
    error2(toofewargs);
  }

  if (map.is_initialized()) {
    uint8_t *map_base = map.map_base();
    Vera_map_size_t width = map.width();
    Vera_map_size_t height = map.height();
    Vera_map_type_t map_type = map.map_type();

    return cons(number((int)map_base),
                cons(number(width),
                     cons(number(height),
                          cons(number(map_type),NULL))));
  }
  else {
    return NULL;
  }
}

object *fn_vera_map_deinit (object *args, object *env) {
  (void) env;

  object *idx = first(args);
  uint32_t idx_checked = checkrange(idx, 0, VERA_NUM_MAPS-1);

  Vera_map &map = vera.map[idx_checked];

  if (!map.is_initialized())
    error(notinitialized, idx);

  map.deinit();

  return nil;
}

object *fn_vera_tileset (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t tileset_id = checkrange(first(args), 0, VERA_NUM_TILESETS-1);

  Vera_tileset &tileset = vera.tileset[tileset_id];

  if (nargs == 5) {
    if (tileset.is_initialized())
      error(alreadyinitialized, first(args));

    bool res = tileset.init(
      checktilesize(second(args)),
      checktilesize(third(args)),
      checkbpp(fourth(args)),
      checkrange(fifth(args), 1, VERA_MAX_NUM_TILES_IN_TILESET));

    if (!res)
      error2(insufficientresources);
  }
  else if (nargs > 1) {
    error2(toofewargs);
  }

  if (tileset.is_initialized()) {
    uint8_t *tileset_base = tileset.tileset_base();
    Vera_tile_size_t width = tileset.width();
    Vera_tile_size_t height = tileset.height();
    Vera_bpp_t bpp = tileset.bpp();
    uint32_t num_tiles = tileset.num_tiles();
    uint32_t tilesize = tileset.tilesize_bytes();

    return cons(number((int)tileset_base),
                cons(number(width),
                     cons(number(height),
                          cons(number(bpp),
                               cons(number(num_tiles),
                                    cons(number(tilesize), NULL))))));
  }
  else {
    return NULL;
  }
}

object *fn_vera_tileset_deinit (object *args, object *env) {
  (void) env;

  object *idx = first(args);
  uint32_t idx_checked = checkrange(idx, 0, VERA_NUM_TILESETS-1);

  Vera_tileset &tileset = vera.tileset[idx_checked];

  if (!tileset.is_initialized())
    error(notinitialized, idx);

  tileset.deinit();

  return nil;
}

object *fn_vera_bitmap (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t bitmap_id = checkrange(first(args), 0, VERA_NUM_BITMAPS-1);

  Vera_bitmap &bitmap = vera.bitmap[bitmap_id];

  if (nargs == 4) {
    if (bitmap.is_initialized())
      error(alreadyinitialized, first(args));

    bool res = bitmap.init(
      checkbitmapwidth(second(args)),
      checkrange(third(args), 1, 0xFFFF),
      checkbpp(fourth(args)));

    if (!res)
      error2(insufficientresources);
  }
  else if (nargs > 1) {
    error2(toofewargs);
  }

  if (bitmap.is_initialized()) {
    uint8_t *bitmap_base = bitmap.bitmap_base();
    Vera_bitmap_width_t width = bitmap.width();
    uint32_t height = bitmap.height();
    Vera_bpp_t bpp = bitmap.bpp();

    return cons(number((int)bitmap_base),
                cons(number(width),
                     cons(number(height),
                          cons(number(bpp),NULL))));
  }
  else {
    return NULL;
  }
}

object *fn_vera_bitmap_deinit (object *args, object *env) {
  (void) env;

  object *idx = first(args);
  uint32_t idx_checked = checkrange(idx, 0, VERA_NUM_BITMAPS-1);

  Vera_bitmap &bitmap = vera.bitmap[idx_checked];

  if (!bitmap.is_initialized())
    error(notinitialized, idx);

  bitmap.deinit();

  return nil;
}

object *fn_vera_line_capture_enable (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  bool line_capture_enable;

  if (nargs == 1) {
    line_capture_enable = checkbitvalue(first(args)) != 0;

    vera.line_capture_enable(line_capture_enable);
  }
  else {
    line_capture_enable = vera.line_capture_enabled();
  }

  return number((int)line_capture_enable);
}

object *fn_vera_line_capture_read_pixel (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *idx = first(args);
  int idxchecked = checkrange(idx, 0, 639);

  Vera_rgb_t rgb = vera.line_capture_read_pixel(idxchecked);

  return cons(number(rgb.r),
              cons(number(rgb.g),
                   cons(number(rgb.b), NULL)));
}

object *fn_vera_spritebank (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  Vera_sprite_bank_t bankchecked;

  if (nargs == 1) {
    object *bank = first(args);

    bankchecked = (Vera_sprite_bank_t)checkrange(bank, 0, 1);

    vera.sprite_bank_set(bankchecked);
  }
  else {
    bankchecked = vera.sprite_bank_get();
  }

  return number((int)bankchecked);
}

object *fn_vera_scanline (object *args, object *env) {
  (void) env;

  return number((int)vera.scanline_get());
}

object *fn_vera_display_enable (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  bool display_enable;

  if (nargs == 1) {
    object *enable = first(args);

    display_enable = checkbitvalue(enable) != 0;

    vera.display_enable(display_enable);
  }
  else {
    display_enable = vera.display_enabled();
  }

  return number((int)display_enable);
}

object *fn_vera_sprite_enable (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  bool sprite_enable;

  if (nargs == 1) {
    object *enable = first(args);

    sprite_enable = checkbitvalue(enable) != 0;

    vera.sprites_enable(sprite_enable);
  }
  else {
    sprite_enable = vera.sprites_enabled();
  }

  return number((int)sprite_enable);
}

object *fn_vera_hscale (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  vera_ufix_1_7_t scalechecked;

  if (nargs == 1) {
    object *scale = first(args);
    float scalefloat = checkrangefloat(scale, 0.0, 1.99);
    scalechecked = (vera_ufix_1_7_t)(scalefloat*128);

    vera.hscale_set(scalechecked);
  }
  else {
    scalechecked = vera.hscale_get();
  }

  return makefloat((float)scalechecked/128.0);
}

object *fn_vera_vscale (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  vera_ufix_1_7_t scalechecked;

  if (nargs == 1) {
    object *scale = first(args);
    float scalefloat = checkrangefloat(scale, 0.0, 1.99);
    scalechecked = (vera_ufix_1_7_t)(scalefloat*128);

    vera.vscale_set(scalechecked);
  }
  else {
    scalechecked = vera.vscale_get();
  }

  return makefloat((float)scalechecked/128.0);
}

object *fn_vera_bordercolor (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint8_t idxchecked;

  if (nargs == 1) {
    object *idx = first(args);

    idxchecked = (uint8_t)checkrange(idx, 0, 255);

    vera.bordercolor_set(idxchecked);
  }
  else {
    idxchecked = vera.bordercolor_get();
  }

  return number((int)idxchecked);
}

object *fn_vera_screen_boundaries (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  Vera_screen_boundaries_t screen_boundaries;

  if (nargs == 4) {
    object *hstart = first(args);
    object *hstop = second(args);
    object *vstart = third(args);
    object *vstop = fourth(args);

    screen_boundaries.hstart = checkrange(hstart, 0, VERA_HSTOP_MAX);
    screen_boundaries.hstop = checkrange(hstop, 0, VERA_HSTOP_MAX);
    screen_boundaries.vstart = checkrange(vstart, 0, VERA_VSTOP_MAX);
    screen_boundaries.vstop = checkrange(vstop, 0, VERA_VSTOP_MAX);

    if (screen_boundaries.hstart >= screen_boundaries.hstop)
      error(invalidarg, hstart);

    if (screen_boundaries.vstart >= screen_boundaries.vstop)
      error(invalidarg, vstart);

    vera.screen_boundaries_set(&screen_boundaries);
  }
  else if (nargs == 0) {
    vera.screen_boundaries_get(&screen_boundaries);
  }
  else {
    error2(toofewargs);
  }

  return cons(number(screen_boundaries.hstart),
              cons(number(screen_boundaries.hstop),
                   cons(number(screen_boundaries.vstart),
                        cons(number(screen_boundaries.vstop), NULL))));
}

object *fn_vera_layer_enable (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  bool layer_enable;

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  if (nargs == 2) {
    object *enable = second(args);

    layer_enable = checkbitvalue(enable) != 0;

    vera.layer[layerchecked].enable(layer_enable);
  }
  else {
    layer_enable = vera.layer[layerchecked].enabled();
  }

  return number((int)layer_enable);
}

object *fn_vera_layer_hscroll (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t offsetchecked;

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  if (nargs == 2) {
    object *offset = second(args);

    offsetchecked = checkrange(offset, 0, 4095);

    vera.layer[layerchecked].hscroll_set(offsetchecked);
  }
  else {
    offsetchecked = vera.layer[layerchecked].hscroll_get();
  }

  return number((int)offsetchecked);
}

object *fn_vera_layer_vscroll (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t offsetchecked;

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  if (nargs == 2) {
    object *offset = second(args);

    offsetchecked = checkrange(offset, 0, 4095);

    vera.layer[layerchecked].vscroll_set(offsetchecked);
  }
  else {
    offsetchecked = vera.layer[layerchecked].vscroll_get();
  }

  return number((int)offsetchecked);
}

object *fn_vera_layer_map (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  uint32_t map_id;

  if (nargs == 2) {
    map_id = checkrange(second(args), 0, VERA_NUM_MAPS-1);

    Vera_map &map = vera.map[map_id];

    if (!map.is_initialized())
      error(notinitialized, second(args));

    vera.layer[layerchecked].map_set(map_id);
  }
  else {
    map_id = vera.layer[layerchecked].map_get();
  }

  return (map_id != ~0U) ? number((int)(map_id)) : NULL;
}

object *fn_vera_layer_tileset (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  uint32_t tileset_id;

  if (nargs == 2) {
    tileset_id = checkrange(second(args), 0, VERA_NUM_TILESETS-1);

    Vera_tileset &tileset = vera.tileset[tileset_id];

    if (!tileset.is_initialized())
      error(notinitialized, second(args));

    vera.layer[layerchecked].tileset_set(tileset_id);
  }
  else {
    tileset_id = vera.layer[layerchecked].tileset_get();
  }

  return (tileset_id != ~0U) ? number((int)(tileset_id)) : NULL;
}

object *fn_vera_layer_bitmap (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  uint32_t bitmap_id;

  if (nargs == 2) {
    bitmap_id = checkrange(second(args), 0, VERA_NUM_BITMAPS-1);

    Vera_bitmap& bitmap = vera.bitmap[bitmap_id];

    if (!bitmap.is_initialized())
      error(notinitialized, second(args));

    vera.layer[layerchecked].bitmap_set(bitmap_id);
  }
  else {
    bitmap_id = vera.layer[layerchecked].bitmap_get();
  }

  return (bitmap_id != ~0U) ? number((int)(bitmap_id)) : NULL;
}

object *fn_vera_layer_pal_offset (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *layer = first(args);
  int layerchecked = checkrange(layer, 0, 1);

  uint8_t pal_offset;

  if (nargs == 2) {
    pal_offset = (uint8_t)checkrange(second(args), 0,  15);

    vera.layer[layerchecked].bitmap_pal_offset_set(pal_offset);
  }
  else {
    pal_offset = vera.layer[layerchecked].bitmap_pal_offset_get();
  }

  return number((uint8_t)(pal_offset));
}

object *fn_vera_palette (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *idx = first(args);
  int idxchecked = checkrange(idx, 0, 255);

  Vera_rgb_t rgb;

  switch (nargs) {
    case 4:
      rgb.r = (uint16_t)checkrange(second(args), 0,  15);
      rgb.g = (uint16_t)checkrange(third(args), 0,  15);
      rgb.b = (uint16_t)checkrange(fourth(args), 0,  15);

      vera.palette.write(idxchecked, rgb);

      break;

    case 3:
    case 2:
      error2(toofewargs);
      break;

    case 1:
      rgb = vera.palette.read_rgb(idxchecked);
      break;

    default:
      vera.palette.restore_default();
  }

  return (nargs == 0) ? NULL :
      cons(number(rgb.r),
              cons(number(rgb.g),
                   cons(number(rgb.b), NULL)));
}

object *fn_vera_sprite_init (object *args, object *env) {
  (void) env;

  object *spriteid = first(args);
  int spriteidchecked = checkrange(spriteid, 0, VERA_NUM_SPRITES-1);

  vera.sprite[spriteidchecked].init();

  return NULL;
}

object *fn_vera_sprite_tile (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  int sprite_id = checkrange(first(args), 0, VERA_NUM_SPRITES-1);

  uint32_t tileset_id;
  uint32_t tile_idx;

  if (nargs == 3) {
    tileset_id = (uint32_t)checkrange(second(args), 0, VERA_NUM_TILESETS);

    Vera_tileset& tileset = vera.tileset[tileset_id];

    if (!tileset.is_initialized())
      error(notinitialized, second(args));

    tile_idx = (uint32_t)checkinteger(third(args));

    if (tile_idx >= tileset.num_tiles())
      error(indexrange, third(args));

    vera.sprite[sprite_id].tile_set(tileset_id, tile_idx);
  }
  else if (nargs == 2) {
    error2(toofewargs);
  }
  else {
    vera.sprite[sprite_id].tile_get(tileset_id, tile_idx);
  }

  return (tileset_id == ~0U) ? NULL :
    cons(number(tileset_id), cons(number(tile_idx), NULL));
}

object *fn_vera_sprite_x (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  uint32_t x;

  if (nargs == 2) {
    x = (uint32_t)checkrange(second(args), 0,  1023);

    vera.sprite[spritechecked].x_set(x);
  }
  else {
    x = vera.sprite[spritechecked].x_get();
  }

  return number((int)x);
}

object *fn_vera_sprite_y (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  uint32_t y;

  if (nargs == 2) {
    y = (uint32_t)checkrange(second(args), 0,  1023);

    vera.sprite[spritechecked].y_set(y);
  }
  else {
    y = vera.sprite[spritechecked].y_get();
  }

  return number((int)y);
}

object *fn_vera_sprite_pal_offset (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  uint8_t pal_offset;

  if (nargs == 2) {
    pal_offset = (uint8_t)checkrange(second(args), 0,  15);

    vera.sprite[spritechecked].pal_offset_set(pal_offset);
  }
  else {
    pal_offset = vera.sprite[spritechecked].pal_offset_get();
  }

  return number((int)pal_offset);
}

object *fn_vera_sprite_col_mask (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  uint8_t col_mask;

  if (nargs == 2) {
    col_mask = (uint8_t)checkrange(second(args), 0,  15);

    vera.sprite[spritechecked].collision_mask_set(col_mask);
  }
  else {
    col_mask = vera.sprite[spritechecked].collision_mask_get();
  }

  return number((int)col_mask);
}

object *fn_vera_sprite_z_depth (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  Vera_z_depth_t z_depth;

  if (nargs == 2) {
    z_depth = (Vera_z_depth_t)checkrange(second(args), VERA_SPR_Z_DIS, VERA_SPR_Z_L1);

    vera.sprite[spritechecked].z_depth_set(z_depth);
  }
  else {
    z_depth = vera.sprite[spritechecked].z_depth_get();
  }

  return number((int)z_depth);
}

object *fn_vera_sprite_hflip (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  bool hflip;

  if (nargs == 2) {
    hflip = checkbitvalue(second(args)) != 0;

    vera.sprite[spritechecked].hflip_set(hflip);
  }
  else {
    hflip = vera.sprite[spritechecked].hflip_get();
  }

  return number((int)hflip);
}

object *fn_vera_sprite_vflip (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  object *sprite = first(args);
  int spritechecked = checkrange(sprite, 0, VERA_NUM_SPRITES-1);

  bool vflip;

  if (nargs == 2) {
    vflip = checkbitvalue(second(args)) != 0;

    vera.sprite[spritechecked].vflip_set(vflip);
  }
  else {
    vflip = vera.sprite[spritechecked].vflip_get();
  }

  return number((int)vflip);
}

object *fn_vera_sprite_pixel (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  int spritechecked = checkrange(first(args), 0, VERA_NUM_SPRITES-1);

  uint32_t x, y;
  uint8_t val;

  Vera_sprite& sprite = vera.sprite[spritechecked];

  x = checkinteger(second(args));
  y = checkinteger(third(args));

  uint32_t tileset_id;
  uint32_t tile_idx;

  sprite.tile_get(tileset_id, tile_idx);

  if (tileset_id >= VERA_NUM_TILESETS)
    error(notinitialized, first(args));

  Vera_tileset& tileset = vera.tileset[tileset_id];

  if (x >= tileset.width())
    error(invalidarg, second(args));

  if (y >= tileset.height())
    error(invalidarg, third(args));

  if (nargs == 4) {
    val = (uint8_t)checkrange(fourth(args), 0, 255);
    sprite.pixel_set(x, y, val);
  }
  else {
    val = sprite.pixel_get(x, y);
  }

  return number((int)val);
}

object *fn_vera_map_entry (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t map_id = checkrange(first(args), 0, VERA_NUM_MAPS-1);

  Vera_map &map = vera.map[map_id];

  if (!map.is_initialized())
    error(notinitialized, first(args));

  Vera_map_size_t width = map.width();
  Vera_map_size_t height = map.height();

  uint16_t entry;
  uint32_t col = checkrange(second(args), 0, width);
  uint32_t row = checkrange(third(args), 0, height);

  if (nargs == 4) {
    entry = (uint16_t)checkrange(fourth(args), 0, 65535);
    map.write(col, row, entry);
  }
  else {
    entry = map.read(col, row);
  }

  return number(entry);
}

object *fn_vera_bitmap_pixel (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t bitmap_id = checkrange(first(args), 0, VERA_NUM_BITMAPS-1);

  Vera_bitmap &bitmap = vera.bitmap[bitmap_id];

  if (!bitmap.is_initialized())
    error(notinitialized, first(args));

  Vera_bitmap_width_t width = bitmap.width();
  uint32_t height = bitmap.height();

  uint8_t val;
  uint32_t x = checkrange(second(args), 0, width);
  uint32_t y = checkrange(third(args), 0, height);

  if (nargs == 4) {
    val = (uint8_t)checkrange(fourth(args), 0, 255);
    bitmap.pixel_set(x, y, val);
  }
  else {
    val = bitmap.pixel_get(x, y);
  }

  return number((int)val);
}

object *fn_vera_tileset_pixel (object *args, object *env) {
  (void) env;

  int nargs = listlength(args);

  uint32_t tileset_id = checkrange(first(args), 0, VERA_NUM_TILESETS-1);

  Vera_tileset &tileset = vera.tileset[tileset_id];

  if (!tileset.is_initialized())
    error(notinitialized, first(args));

  Vera_tile_size_t width = tileset.width();
  Vera_tile_size_t height = tileset.height();
  uint32_t num_tiles = tileset.num_tiles();

  uint8_t val;
  uint32_t tile_idx = checkrange(second(args), 0, num_tiles);
  uint32_t x = checkrange(third(args), 0, width);
  uint32_t y = checkrange(fourth(args), 0, height);

  if (nargs == 5) {
    val = (uint8_t)checkrange(fifth(args), 0, 255);
    tileset.pixel_set(tile_idx, x, y, val);
  }
  else {
    val = tileset.pixel_get(tile_idx, x, y);
  }

  return number((int)val);
}

// Symbol names
const char stringvera_init[] = "vera_init";
const char stringvera_map[] = "vera_map";
const char stringvera_map_deinit[] = "vera_map_deinit";
const char stringvera_tileset[] = "vera_tileset";
const char stringvera_tileset_deinit[] = "vera_tileset_deinit";
const char stringvera_bitmap[] = "vera_bitmap";
const char stringvera_bitmap_deinit[] = "vera_bitmap_deinit";
const char stringvera_line_capture_enable[] = "vera_line_capture_enable";
const char stringvera_line_capture_read_pixel[] = "vera_line_capture_read_pixel";
const char stringvera_spritebank[] = "vera_spritebank";
const char stringvera_scanline[] = "vera_scanline";
const char stringvera_display_enable[] = "vera_display_enable";
const char stringvera_sprite_enable[] = "vera_sprite_enable";
const char stringvera_hscale[] = "vera_hscale";
const char stringvera_vscale[] = "vera_vscale";
const char stringvera_bordercolor[] = "vera_bordercolor";
const char stringvera_screen_boundaries[] = "vera_screen_boundaries";
const char stringvera_layer_enable[] = "vera_layer_enable";
const char stringvera_layer_hscroll[] = "vera_layer_hscroll";
const char stringvera_layer_vscroll[] = "vera_layer_vscroll";
const char stringvera_layer_map[] = "vera_layer_map";
const char stringvera_layer_tileset[] = "vera_layer_tileset";
const char stringvera_layer_bitmap[] = "vera_layer_bitmap";
const char stringvera_layer_pal_offset[] = "vera_layer_pal_offset";
const char stringvera_palette[] = "vera_palette";
const char stringvera_sprite_init[] = "vera_sprite_init";
const char stringvera_sprite_tile[] = "vera_sprite_tile";
const char stringvera_sprite_x[] = "vera_sprite_x";
const char stringvera_sprite_y[] = "vera_sprite_y";
const char stringvera_sprite_pal_offset[] = "vera_sprite_pal_offset";
const char stringvera_sprite_col_mask[] = "vera_sprite_col_mask";
const char stringvera_sprite_z_depth[] = "vera_sprite_z_depth";
const char stringvera_sprite_hflip[] = "vera_sprite_hflip";
const char stringvera_sprite_vflip[] = "vera_sprite_vflip";
const char stringvera_sprite_pixel[] = "vera_sprite_pixel";
const char stringvera_map_entry[] = "vera_map_entry";
const char stringvera_bitmap_pixel[] = "vera_bitmap_pixel";
const char stringvera_tileset_pixel[] = "vera_tileset_pixel";

// Documentation strings
const char docvera_init[] = "(vera_init)\n"
"(Re)Initialize Vera.\n"
"@return: nil\n";

const char docvera_map[] = "(vera_map map_id [width height map_type])\n"
"Initialize a map object, allocate VRAM resources. If only map_id is given,\n"
"retrieve map properties.\n"
"@param map_id: id of map object to initialize. Range: 0..VERA_NUM_MAPS-1.\n"
"@param width: width in tiles: 32/64/128/256.\n"
"@param height: height in tiles: 32/64/128/256.\n"
"@param map_type: TXT16/TXT256/TILE.\n"
"@return: (map_base, width, height, map_type) or nil if map is not initialized.\n";

const char docvera_map_deinit[] = "(vera_map_deinit map_id)\n"
"Deinit map object, releasing its VRAM resources.\n"
"@param map: id of map object to deinit. Range: 0..VERA_NUM_MAPS-1.\n"
"@return: nil.\n";

const char docvera_tileset[] = "(vera_tileset tileset_id [width height bpp num_tiles])\n"
"Initialize a tileset object using given parameters. If only tileset_id is given,\n"
"retrieve tileset properties.\n"
"A tileset can be used to represent both tilesets and spritesets.\n"
"@param tileset: id of tileset object. Range: 0..VERA_NUM_TILESETS-1.\n"
"@param width: 8/16 for non-sprite tiles. For sprites, additionally 32 and 64 are allowed.\n"
"@param height: 8/16 for non-sprite tiles. For sprites, additionally 32 and 64 are allowed.\n"
"@param bpp: 1/2/4/8. In case of spritesets, only 4 and 8 are allowed.\n"
"@param num_tiles: Number of tiles in the tileset. Range: 0..1023.\n"
"@return: (tileset_base, width, height, bpp, num_tiles, tilesize_bytes) or nil if tileset is not initialized.\n";

const char docvera_tileset_deinit[] = "(vera_tileset_deinit tileset_id)\n"
"Deinit tileset object, releasing its VRAM resources.\n"
"@param tileset: id of tileset object to deinit. Range: 0..VERA_NUM_TILESETS-1.\n"
"@return: nil.\n";

const char docvera_bitmap[] = "(vera_bitmap bitmap_id [width height bpp])\n"
"Initialize a bitmap object using given parameters. If only bitmap_id is given,\n"
"retrieve bitmap properties.\n"
"@param bitmap: id of bitmap object to initialize.\n"
"@param width: 320/640\n"
"@param height: in pixels.\n"
"@param bpp: Supported values: 1/2/4/8.\n"
"@return: (bitmap_base, width, height, bpp) or nil if bitmap is not initialized.\n";

const char docvera_bitmap_deinit[] = "(vera_bitmap_deinit bitmap)\n"
"Deinitialize bitmap object, releasing its VRAM resources.\n"
"@param bitmap: id of bitmap object to deinit.\n"
"@return: nil.\n";

const char docvera_line_capture_enable[] = "(vera_line_capture_enable [enable])\n"
"Enable VGA line capture at the line IRQ scanline and retrieve the line capture pending state.\n"
"@param enable: 1/0 enables/disables VGA line capture.\n"
"@return: 1 if line capture is pending, 0 if line capture has been completed/is disabled.\n";

const char docvera_line_capture_read_pixel[] = "(vera_line_capture_read_pixel x\n"
"Read the RGB value of a pixel from the captured VGA line.\n"
"@param x: Range: 0..639.\n"
"@return: (r, g, b).\n";

const char docvera_spritebank[] = "(vera_spritebank [bank])\n"
"Set or get the active spritebank.\n"
"@param bank: the active spritebank. Range: 0..1.\n"
"@return: the active spritebank.\n";

const char docvera_scanline[] = "(vera_scanline)\n"
"@return: the current VGA scanline.\n";

const char docvera_display_enable[] = "(vera_display_enable [enable])\n"
"Enable/disable the display and/or retrieve the current display state.\n"
"@param enable: 1/0 enables/disables the display.\n"
"@return: 1/0 if the display is enabled/disabled.\n";

const char docvera_sprite_enable[] = "(vera_sprite_enable [enable])\n"
"Enable/disable the sprite renderer and/or retrieve the current sprite renderer state.\n"
"@param enable: 1/0 enables/disables the sprite renderer.\n"
"@return: 1/0 if the sprite renderer is enabled/disabled.\n";

const char docvera_hscale[] = "(vera_hscale [scalefactor])\n"
"Set or get the horizontal scalefactor.\n"
"@param scalefactor: The horizontal scale factor. Floating point value.\n"
"1.0 means 1 output pixel for 1 input pixel. 0.5 means 2 output pixels for 1 input pixel (zoom in).\n"
"@return: the horizontal scalefactor.\n";

const char docvera_vscale[] = "(vera_vscale [scalefactor])\n"
"Set or get the vertical scalefactor.\n"
"@param scalefactor: The vertical scale factor. Floating point value.\n"
"1.0 means 1 output pixel for 1 input pixel. 0.5 means 2 output pixels for 1 input pixel (zoom in).\n"
"@return: the horizontal scalefactor.\n";

const char docvera_bordercolor[] = "(vera_bordercolor [color_index])\n"
"Set or get the bordercolor.\n"
"@param colorindex: the palette index of the border color.\n"
"@return: the bordercolor index.\n";

const char docvera_screen_boundaries[] = "(vera_screen_boundaries [hstart hstop, vstart, vstop]])\n"
"Set or get the screen boundaries.\n"
"@param hstart: Range: 0..hstop-1\n"
"@param hstop: Range: hstart+1..1023\n"
"@param vstart: Range: 0..vstop-1\n"
"@param vstop: Range: vsitart+1..1023\n"
"@return: (hstart hstop vstart vstop).\n";

const char docvera_layer_enable[] = "(vera_layer_enable layer [enable])\n"
"Enable/disable the given layer and/or retrieve the current layer state.\n"
"@param layer: 1/0: the layer id.\n"
"@param enable: 1/0: enables/disables the layer.\n"
"@return: 1/0: if the given layer is enabled/disabled.\n";

const char docvera_layer_hscroll[] = "(vera_layer_hscroll layer [offset])\n"
"Set or get the layer's horizontal scroll offset.\n"
"@param layer: 1/0: the layer id.\n"
"@param offset: 0.255 in bitmap mode, 0..4095 in tile mode.\n"
"@return: the layer's horizontal scroll offset.\n";

const char docvera_layer_vscroll[] = "(vera_layer_vscroll layer [offset])\n"
"Set or get the layer's vertical scroll offset.\n"
"@param layer: 1/0: the layer id.\n"
"@param offset: 0.255 in bitmap mode, 0..4095 in tile mode.\n"
"@return: the layer's vertical scroll offset.\n";

const char docvera_layer_map[] = "(vera_layer_map layer [map_id])\n"
"Set or get the layer's map.\n"
"@param layer: 1/0: the layer id.\n"
"@param map_id: the id of the map object whose properties to use. Range: 0..VERA_NUM_MAPS-1.\n"
"@return: the layer's current map_id or nil if no map is currently set.\n";

const char docvera_layer_tileset[] = "(vera_layer_tileset layer [tileset_id])\n"
"Put the layer in tilemode using the given tileset's properties.\n"
"@param layer: 1/0: the layer id.\n"
"@param tileset_id: the id of the tile object whose properties to use. Range: 0..VERA_NUM_TILESETS-1.\n"
"@return: the layer's current tileset_id or nil if no tileset is currently set.\n";

const char docvera_layer_bitmap[] = "(vera_layer_bitmap layer [bitmap_id])\n"
"Put the layer in bitmapmode using the given bitmap object's properties.\n"
"@param layer: 1/0: the layer id.\n"
"@param bitmap_id: the id of the bitmap object whose properties to use. Range: 0..VERA_NUM_BITMAPS-1.\n"
"@return: the layer's current bitmap_id or nil if no bitmap is currently set.\n";

const char docvera_layer_pal_offset[] = "(vera_layer_pal_offset layer [offset])\n"
"Assuming bitmap mode, set or get the layer's palette offset.\n"
"@param layer: 1/0: the layer id.\n"
"@param offset: the palette offset to use. Range: 0..15.\n"
"The color index of bitmap pixels is modified by the palette offset using the following logic:\n"
"- Color index 0 (transparent) and 16-255 are unmodified.\n"
"- Color index 1-15 is modified by adding 16 x palette offset.\n"
"@return: the layer's current palette offset.\n";

const char docvera_palette[] = "(vera_palette [idx [r g b]])\n"
"Set or get the rgb value at the given palette index.\n"
"If not parameters are given, the default palette is restored.\n"
"@param idx: the paletter color index. Range: 0..255.\n"
"@param r: The red component intensity. Range: 0..15.\n"
"@param g: The green component intensity. Range: 0..15.\n"
"@param b: The blue component intensity. Range: 0..15.\n"
"@return: (r g b) or nil if invoked without parameters.\n";

const char docvera_sprite_init[] = "(vera_sprite_init sprite_id)\n"
"(Re)Init the sprite object.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@return: nil\n";

const char docvera_sprite_tile[] = "(vera_sprite_tile sprite_id [tileset_id tile_idx])\n"
"Get or set the sprite's tile containing the sprite's pixel data.\n"
"Attributes width, height and bpp are taken from the tileset.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param tileset_id: tileset with which this sprite is associated. Range: 0..VERA_NUM_TILESETS-1.\n"
"@param tile_idx: the index of the tile in the tileset containing the sprite's pixel\n"
"data. Range: 0..tileset.num_tiles.\n"
"@return: (tileset_id, tile_idx) or nil if currently no tile is configured for this sprite\n";

const char docvera_sprite_x[] = "(vera_sprite_tile sprite_id [x])\n"
"Get or set the sprite's x position.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param x: Range: 0..1023.\n"
"@return: the sprite's x position\n";

const char docvera_sprite_y[] = "(vera_sprite_tile sprite_id [y])\n"
"Get or set the sprite's y position.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param y: Range: 0..1023.\n"
"@return: the sprite's y position\n";

const char docvera_sprite_pal_offset[] = "(vera_sprite_pal_offset sprite_id [pal_offset])\n"
"Get or set the sprite's palette offset.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param offset: the palette offset to use. Range: 0..15.\n"
"The color index of sprite pixels is modified by the palette offset using the following logic:\n"
"- Color index 0 (transparent) and 16-255 are unmodified.\n"
"- Color index 1-15 is modified by adding 16 x palette offset.\n"
"@return: the sprite's current palette offset.\n";

const char docvera_sprite_col_mask[] = "(vera_sprite_col_mask sprite_id [mask])\n"
"Get or set the sprite's collision mask.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param mask: the collision mask. Range: 0..15.\n"
"@return: the sprite's current collision_mask.\n";

const char docvera_sprite_z_depth[] = "(vera_sprite_z_depth sprite_id [z_depth])\n"
"Get or set the sprite's z_depth.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param z_depth: VERA_SPR_Z_DIS/BG_L0/L0_L1/L1.\n"
"@return: the sprite's current z_depth.\n";

const char docvera_sprite_hflip[] = "(vera_sprite_hflip sprite_id [hflip])\n"
"Get or set the sprite's hflip state.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param hflip: 0 or 1.\n"
"@return: the sprite's current hflip value.\n";

const char docvera_sprite_vflip[] = "(vera_sprite_vflip sprite_id [vflip])\n"
"Get or set the sprite's vflip state.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param vflip: 0 or 1.\n"
"@return: the sprite's current vflip value.\n";

const char docvera_sprite_pixel[] = "(vera_sprite_pixel sprite_id x y [val])\n"
"Get or set a pixel in a sprite.\n"
"@param sprite_id: the sprite_id. Range: 0..127.\n"
"@param x: Range: 0..sprite width-1.\n"
"@param y: Range: 0..sprite height-1.\n"
"@param val: the pixel value. Range: 0..(2^bpp)-1.\n"
"@return: the pixel value.\n";

const char docvera_map_entry[] = "(vera_map_entry map_id col row [entry])\n"
"Set or get a 16-bit map entry.\n"
"@param map_id. Range: 0..VERA_NUM_MAPS-1.\n"
"@param col: the map column. Range: 0..map width-1.\n"
"@param row: the map row. Range: 0..map height-1.\n"
"@param entry: the 16-bit entry value.\n"
"@return: 16-bit map entry.\n";

const char docvera_bitmap_pixel[] = "(vera_bitmap_pixel bitmap_id x y [val])\n"
"Set or get a bitmap pixel.\n"
"@param bitmap_id. Range: 0..VERA_NUM_BITMAPS-1.\n"
"@param x: the pixel x position. Range: 0..bitmap width-1.\n"
"@param y: the pixel y position. Range: 0..bitmap height-1.\n"
"@param val: the pixel value. Range: 0..(2^bpp)-1.\n"
"@return: pixel value\n";

const char docvera_tileset_pixel[] = "(vera_tileset_pixel tilset_id tile_idx x y [val])\n"
"Set or get a tile pixel.\n"
"@param tileset_id.\n"
"@param tile_idx: Index into the tileset. Range: 0..num_tiles-1.\n"
"@param x: the pixel x position. Range: 0..tile width-1.\n"
"@param y: the pixel y position. Range: 0..tile height-1.\n"
"@param val: the pixel value. Range: 0..(2^bpp)-1.\n"
"@return: pixel value\n";

const tbl_entry_t lookup_table2[] = {
  { stringvera_init, fn_vera_init, 0200, docvera_init },
  { stringvera_map, fn_vera_map, 0214, docvera_map },
  { stringvera_map_deinit, fn_vera_map_deinit, 0211, docvera_map_deinit },
  { stringvera_tileset, fn_vera_tileset, 0215, docvera_tileset },
  { stringvera_tileset_deinit, fn_vera_tileset_deinit, 0211, docvera_tileset_deinit },
  { stringvera_bitmap, fn_vera_bitmap, 0214, docvera_bitmap },
  { stringvera_bitmap_deinit, fn_vera_bitmap_deinit, 0211, docvera_bitmap_deinit },
  { stringvera_line_capture_enable, fn_vera_line_capture_enable, 0201, docvera_line_capture_enable},
  { stringvera_line_capture_read_pixel, fn_vera_line_capture_read_pixel, 0201, docvera_line_capture_read_pixel},
  { stringvera_sprite_enable, fn_vera_sprite_enable, 0201, docvera_sprite_enable},
  { stringvera_spritebank, fn_vera_spritebank, 0201, docvera_spritebank},
  { stringvera_scanline, fn_vera_scanline, 0200, docvera_scanline},
  { stringvera_display_enable, fn_vera_display_enable, 0201, docvera_display_enable},
  { stringvera_hscale, fn_vera_hscale, 0201, docvera_hscale},
  { stringvera_hscale, fn_vera_vscale, 0201, docvera_vscale},
  { stringvera_bordercolor, fn_vera_bordercolor, 0201, docvera_bordercolor},
  { stringvera_screen_boundaries, fn_vera_screen_boundaries, 0204, docvera_screen_boundaries},
  { stringvera_layer_enable, fn_vera_layer_enable, 0212, docvera_layer_enable},
  { stringvera_layer_hscroll, fn_vera_layer_hscroll, 0212, docvera_layer_hscroll},
  { stringvera_layer_vscroll, fn_vera_layer_vscroll, 0212, docvera_layer_vscroll},
  { stringvera_layer_map, fn_vera_layer_map, 0212, docvera_layer_map},
  { stringvera_layer_tileset, fn_vera_layer_tileset, 0212, docvera_layer_tileset},
  { stringvera_layer_bitmap, fn_vera_layer_bitmap, 0212, docvera_layer_bitmap},
  { stringvera_layer_pal_offset, fn_vera_layer_pal_offset, 0212, docvera_layer_pal_offset},
  { stringvera_palette, fn_vera_palette, 0204, docvera_palette},
  { stringvera_sprite_init, fn_vera_sprite_init, 0211, docvera_sprite_init},
  { stringvera_sprite_tile, fn_vera_sprite_tile, 0213, docvera_sprite_tile},
  { stringvera_sprite_x, fn_vera_sprite_x, 0212, docvera_sprite_x},
  { stringvera_sprite_y, fn_vera_sprite_y, 0212, docvera_sprite_y},
  { stringvera_sprite_pal_offset, fn_vera_sprite_pal_offset, 0212, docvera_sprite_pal_offset},
  { stringvera_sprite_col_mask, fn_vera_sprite_col_mask, 0212, docvera_sprite_col_mask},
  { stringvera_sprite_z_depth, fn_vera_sprite_z_depth, 0212, docvera_sprite_z_depth},
  { stringvera_sprite_hflip, fn_vera_sprite_hflip, 0212, docvera_sprite_hflip},
  { stringvera_sprite_vflip, fn_vera_sprite_vflip, 0212, docvera_sprite_vflip},
  { stringvera_sprite_pixel, fn_vera_sprite_pixel, 0234, docvera_sprite_pixel},
  { stringvera_map_entry, fn_vera_map_entry, 0234, docvera_map_entry},
  { stringvera_bitmap_pixel, fn_vera_bitmap_pixel, 0234, docvera_bitmap_pixel},
  { stringvera_tileset_pixel, fn_vera_tileset_pixel, 0245, docvera_tileset_pixel},
};

// Table cross-reference functions

tbl_entry_t *tables[] = {lookup_table, lookup_table2};
const int tablesizes[] = { arraysize(lookup_table), arraysize(lookup_table2) };

const tbl_entry_t *table (int n) {
  return tables[n];
}

unsigned int tablesize (int n) {
  return tablesizes[n];
}
