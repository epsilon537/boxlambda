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
inline void vram_wr(uint32_t addr, uint32_t data) {
  *(volatile uint32_t *)(addr+VERA_VRAM_BASE) = data;
}

//VERA VRAM byte write
inline void vram_wr_byte(uint32_t addr, uint8_t data) {
  *(volatile uint8_t *)(addr+VERA_VRAM_BASE) = data;
}

//VERA VRAM word read
inline uint32_t vram_rd(uint32_t addr) {
  return (*(volatile uint32_t *)(addr+VERA_VRAM_BASE));
}

//VERA VRAM byte read
inline uint8_t vram_rd_byte(uint32_t addr) {
  return (*(volatile uint8_t *)(addr+VERA_VRAM_BASE));
}

//This function writes the given rgb triple to the given position in VERA's Palette RAM.
inline void palette_ram_wr(uint32_t idx, uint8_t r, uint8_t g, uint8_t b) {
  *(volatile uint32_t *)(idx*4+VERA_PALETTE_BASE) = (((uint32_t)r)<<8) | (((uint32_t)g)<<4) | ((uint32_t)b);
}

//Crude sprite attributes write function.
void sprite_attr_wr(uint32_t sprite_id, uint32_t addr, uint32_t mode, uint32_t x, uint32_t y,
                    uint32_t z, uint32_t collision_mask, uint32_t width, uint32_t height);

#ifdef __cplusplus
}
#endif
#endif //VERA_HAL_H
