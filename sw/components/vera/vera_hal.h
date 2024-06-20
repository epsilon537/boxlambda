#ifndef VERA_HAL_H
#define VERA_HAL_H

#include "vera.h"
#include "interrupts.h"

#define VERA_BASE 0x12000000
#define VERA_VRAM_BASE (VERA_BASE + VERA_VRAM_OFFSET)
#define VERA_PALETTE_BASE (VERA_BASE + VERA_PALETTE_OFFSET)
#define VERA_SPRITES_BASE (VERA_BASE + VERA_SPRITES_OFFSET)

/* Tie the VERA IRQ to the RM_1 (Reconfigurable Module 1) IRQ handler. */
#define IRQ_ID_VERA IRQ_ID_RM_1

#define _vera_irq_handler _rm_1_irq_handler

/*A low-level Hardware Access Layer for VERA.*/
//VERA register write
inline void vera_reg_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(VERA_BASE + addr) = data;
}

//VERA register read
inline unsigned vera_reg_rd(unsigned addr) {
  return *(unsigned volatile *)(VERA_BASE + addr);
}

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

#endif //VERA_HAL_H
