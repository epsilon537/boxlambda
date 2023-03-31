#include "vera_hal.h"

//A very crude wishbone bus write implementation.
void vera_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(addr) = data;
}

//A very crude wishbone bus read implementation.
unsigned char vera_rd(unsigned addr) {
  return (unsigned char)(*(volatile unsigned *)(addr));  
}

//This function writes the given data word to the given address in VERA's VRAM.
void vram_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(addr+VERA_VRAM_BASE) = data;
}

//This function writes the given data byte to the given address in VERA's VRAM.
void vram_wr_byte(unsigned addr, unsigned char data) {
  *(volatile unsigned char *)(addr+VERA_VRAM_BASE) = data;
}

unsigned vram_rd(unsigned addr) {
  return (*(volatile unsigned *)(addr+VERA_VRAM_BASE));  
}

unsigned char vram_rd_byte(unsigned addr) {
  return (*(volatile unsigned char *)(addr+VERA_VRAM_BASE));  
}

//This function writes the given rgb triple to the given position in VERA's Palette RAM.
void palette_ram_wr(unsigned idx, unsigned char r, unsigned char g, unsigned char b) {
  *(volatile unsigned *)(idx*4+VERA_PALETTE_BASE) = (((unsigned)r)<<8) | (((unsigned)g)<<4) | ((unsigned)b);
}

//This function writes the given data word to the given address in VERA's Sprite RAM.
void sprite_ram_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(addr+VERA_SPRITES_BASE) = data;
}

