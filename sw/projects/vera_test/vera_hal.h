#ifndef VERA_HAL_H
#define VERA_HAL_H

#include "vera.h"

//A very crude wishbone bus write implementation.
void vera_wr(unsigned addr, unsigned data);

//A very crude wishbone bus read implementation.
unsigned char vera_rd(unsigned addr);

void vram_wr(unsigned addr, unsigned data);

void vram_wr_byte(unsigned addr, unsigned char data);

unsigned vram_rd(unsigned addr);

unsigned char vram_rd_byte(unsigned addr);

void palette_ram_wr(unsigned idx, unsigned char r, unsigned char g, unsigned char b);

void sprite_ram_wr(unsigned addr, unsigned data);


#endif /*VERA_HAL_H*/
