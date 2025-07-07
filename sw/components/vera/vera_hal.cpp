#include "vera_hal.h"

//Crude sprite attributes write function.
void sprite_attr_wr(uint32_t sprite_id, uint32_t addr, uint32_t mode, uint32_t x, uint32_t y,
                    uint32_t z, uint32_t collision_mask, uint32_t width, uint32_t height) {
  uint32_t v,w;

  v = (addr>>5);
  v |= (mode<<15);
  v |= (x<<16); //x
  w = y; //y
  w |= (z<<18); //z
  w |= (collision_mask<<20);
  w |= (width<<28); //width:64
  w |= (height<<30); //height

  *(volatile uint32_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8) = v;
  *(volatile uint32_t *)(VERA_SPRITE_RAM_BASE + sprite_id*8 + 4) = w;
}
