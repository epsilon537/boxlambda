#ifndef VERA_HAL_H
#define VERA_HAL_H

#include "vera_regs.h"

#define VERA_PALETTE_OFFSET (0x2000)
#define VERA_SPRITES_OFFSET (0x1000)
#define VERA_VRAM_OFFSET   (0x40000)
#define VERA_VRAM_BASE (VERA_BASE_ADDR + VERA_VRAM_OFFSET)
#define VERA_PALETTE_BASE (VERA_BASE_ADDR + VERA_PALETTE_OFFSET)
#define VERA_SPRITES_BASE (VERA_BASE_ADDR + VERA_SPRITES_OFFSET)

#ifdef __cplusplus
extern "C" {
#endif

//VERA VRAM word write
inline void vram_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(addr+VERA_VRAM_BASE) = data;
}

//VERA VRAM byte write
inline void vram_wr_byte(unsigned addr, unsigned char data) {
  *(volatile unsigned char *)(addr+VERA_VRAM_BASE) = data;
}

//VERA VRAM word read
inline unsigned vram_rd(unsigned addr) {
  return (*(volatile unsigned *)(addr+VERA_VRAM_BASE));
}

//VERA VRAM byte read
inline unsigned char vram_rd_byte(unsigned addr) {
  return (*(volatile unsigned char *)(addr+VERA_VRAM_BASE));
}

//This function writes the given rgb triple to the given position in VERA's Palette RAM.
inline void palette_ram_wr(unsigned idx, unsigned char r, unsigned char g, unsigned char b) {
  *(volatile unsigned *)(idx*4+VERA_PALETTE_BASE) = (((unsigned)r)<<8) | (((unsigned)g)<<4) | ((unsigned)b);
}

//Crude sprite attributes write function.
void sprite_attr_wr(unsigned sprite_id, unsigned addr, unsigned mode, unsigned x, unsigned y,
                    unsigned z, unsigned collision_mask, unsigned width, unsigned height);

#ifdef __cplusplus
}
#endif
#endif //VERA_HAL_H
