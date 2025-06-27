#include "vera_hal.h"

//Crude sprite attributes write function.
void sprite_attr_wr(unsigned sprite_id, unsigned addr, unsigned mode, unsigned x, unsigned y,
                    unsigned z, unsigned collision_mask, unsigned width, unsigned height) {
  unsigned v,w;

  v = (addr>>5);
  v |= (mode<<15);
  v |= (x<<16); //x
  w = y; //y
  w |= (z<<18); //z
  w |= (collision_mask<<20);
  w |= (width<<28); //width:64
  w |= (height<<30); //height

  *(volatile unsigned *)(VERA_SPRITES_BASE + sprite_id*8) = v;
  *(volatile unsigned *)(VERA_SPRITES_BASE + sprite_id*8 + 4) = w;
}
