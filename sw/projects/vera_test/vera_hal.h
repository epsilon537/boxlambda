#ifndef VERA_HAL_H
#define VERA_HAL_H

/*A low-level Hardware Access Layer for VERA.*/
//VERA register write
inline void vera_reg_wr(unsigned addr, unsigned data) {
  *(volatile unsigned *)(addr) = data;
}

//VERA register read
inline unsigned vera_reg_rd(unsigned addr) {
  return *(unsigned volatile *)(addr);  
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
inline void sprite_attr_wr(unsigned sprite_id, unsigned addr, unsigned mode, unsigned x, unsigned y, 
                    unsigned z, unsigned width, unsigned height) {
  unsigned v,w;

  v = (addr>>5);
  v |= (mode<<15);
  v |= (x<<16); //x
  w = y; //y
  w |= (z<<18); //z
  w |= (width<<28); //width:64
  w |= (height<<30); //height

  *(volatile unsigned *)(VERA_SPRITES_BASE + sprite_id*8) = v;
  *(volatile unsigned *)(VERA_SPRITES_BASE + sprite_id*8 + 4) = w;
}

#endif //VERA_HAL_H