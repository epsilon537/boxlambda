#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdram.h"
#include "vera_hal.h"
#include "interrupts.h"

#define VRAM_MAP_BASE (0x8000) //Relative to VERA base address.

volatile uint32_t frame_counter = 0;
volatile uint32_t vsync_irq_fired=0;
volatile uint32_t line_irq_fired=0;
volatile uint32_t sprcol_irq_fired=0;

void _vera_irq_handler(void) {
  uint32_t isr = VERA->ISR;
  uint32_t ien = VERA->IEN;

  if (isr & ien & VERA_ISR_VSYNC_MASK) {
    ++frame_counter;
    //Switch to sprite bank 0 at vsync
    VERA->CTRL = 0;
    vsync_irq_fired = 1;
  }

  if (isr & ien & VERA_ISR_LINE_MASK) {
    line_irq_fired = 1;
    //Switch to sprite bank 1 at scanline 240
    VERA->CTRL = 1;
  }

  if (isr & ien & VERA_ISR_SPRCOL_MASK) {
    sprcol_irq_fired = 1;
  }

  //Acknowledge IRQ in the VERA core.
  VERA->ISR = isr&ien;

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

//Returns 0 if OK, <0 if error.
int generate_8bpp_8x8_tiles() {
  uint8_t data=0;

  //Just generate 8x8 blocks of different colors
  for (int jj=0;jj<16;jj++) {
    for (int ii=0; ii<64; ii++) {
      vram_wr_byte(jj*64+ii, (ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0);

      data = vram_rd_byte(jj*64+ii);

      if (data != ((ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0)) {
        printf("VRAM read back mismatch addr: 0x%x: 0x%x vs. 0x%x.\n\r", jj*64+ii, data, ((ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0));
        return -1;
      }
    }
  }

  printf("VRAM read back OK.\n\r");
  return 0;
}

void generate_8bpp_64x64_sprite(uint32_t vram_addr) {
  for (int ii=0; ii<64*64/4; ii++) {
      vram_wr(vram_addr+ii*4, 0x03030303);
    }
}

void setup_sprite_attr_ram(int collide) {
  int i;
  uint32_t v,w;

  for (i=0; i<64; i++) {
    sprite_attr_wr(i, 0x1c0, 1, collide ? 4*i : 8*i, 16, 3, 1, 0, 0);
  }

  for (i=0; i<64; i++) {
    sprite_attr_wr(i+64, 0x1000, 1, collide ? 35*i : 70*i, 300, 3, 1, 3, 3);
  }
}

void setup_palette_ram(void) {
  for (uint32_t ii=0; ii<256; ii++) {
    palette_ram_wr(ii, ((ii>>4)&3)<<2, ((ii>>2)&3)<<2, (ii&3)<<2);
  }
}

//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);
  disable_all_irqs();
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().

int main(void) {
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs

  uint32_t read_back_val=0;
  uint32_t read_back_err=0;

  printf("Setting up VERA registers...\n");

  uint32_t dc_video_reg = 0x71; //sprite enable, Layer 1 enable, Layer 0 enable, VGA output mode.
  VERA->DC_VIDEO = dc_video_reg;
  read_back_val = VERA->DC_VIDEO;

  if (read_back_val != dc_video_reg) {
    printf("VERA_DC_VIDEO read back incorrectly: 0x%x, expected 0x%x\n\r", read_back_val, dc_video_reg);
    read_back_err = 1;
  }
  else {
    printf("VERA_DC_VIDEO read back OK\n\r");
  }

  vera_l0_config_t l0_config;
  l0_config.COLOR_DEPTH = VERA_L0_CONFIG_COLOR_DEPTH_EIGHT_BPP;
  l0_config.BITMAP_MODE = 0;
  l0_config.MAP_WIDTH = VERA_L0_CONFIG_MAP_WIDTH_TILES_128;
  l0_config.MAP_HEIGHT = VERA_L0_CONFIG_MAP_HEIGHT_TILES_128;
  VERA->L0_CONFIG = l0_config.UINT32;
  vera_l0_tilebase_t l0_tilebase;
  l0_tilebase.TILE_WIDTH = VERA_L0_TILEBASE_TILE_WIDTH_TILE_WIDTH_8;
  l0_tilebase.TILE_HEIGHT = VERA_L0_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_8;
  l0_tilebase.TILE_BASE_ADDR_16_11 = 0;
  VERA->L0_TILEBASE = l0_tilebase.UINT32;
  VERA->L0_MAPBASE = VRAM_MAP_BASE>>9; //Map base address 0x10000
  VERA->L0_HSCROLL = 4;
  VERA->L0_VSCROLL = 4;

  vera_l1_config_t l1_config;
  l1_config.COLOR_DEPTH = VERA_L1_CONFIG_COLOR_DEPTH_EIGHT_BPP;
  l1_config.BITMAP_MODE = 0;
  l1_config.MAP_WIDTH = VERA_L1_CONFIG_MAP_WIDTH_TILES_128;
  l1_config.MAP_HEIGHT = VERA_L1_CONFIG_MAP_HEIGHT_TILES_128;
  VERA->L1_CONFIG = l1_config.UINT32;
  vera_l1_tilebase_t l1_tilebase;
  l1_tilebase.TILE_WIDTH = VERA_L1_TILEBASE_TILE_WIDTH_TILE_WIDTH_8;
  l1_tilebase.TILE_HEIGHT = VERA_L1_TILEBASE_TILE_HEIGHT_TILE_HEIGHT_8;
  l1_tilebase.TILE_BASE_ADDR_16_11 = 0;
  VERA->L1_TILEBASE = l1_tilebase.UINT32;
  VERA->L1_MAPBASE = VRAM_MAP_BASE>>9; //Map base address 0x10000
  VERA->L1_HSCROLL = 4;
  VERA->L1_VSCROLL = 4;

  VERA->CTRL_bf.SBNK = 0; //Sprite Bank 0

  VERA->DC_HSCALE = 92;
  VERA->DC_VSCALE = 92;

  printf("Setting up VRAM...\n");

  if (generate_8bpp_8x8_tiles() < 0)
    read_back_err = 1;

  generate_8bpp_64x64_sprite(/*vram_addr=*/0x1000);
  setup_sprite_attr_ram(0);
  setup_palette_ram();

  //Fill VRAM map area
  for (int ii=0; ii<128*128*2; ii+=2) {
    vram_wr_byte(VRAM_MAP_BASE+ii, (uint8_t)ii&0xf);
    vram_wr_byte(VRAM_MAP_BASE+ii+1, 0);
  }

  if (read_back_err == 0) {
    printf("VERA Read Back Tests successful.\n");
  }
  else {
    printf("VERA Read Back Tests failed.\n");
  }

  VERA->IRQLINE = 240;

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable global IRQ line in the CPU
  enable_irq(IRQ_ID_VERA); //Enable the VERA IRQ in the CPU
  VERA->ISR = VERA_ISR_VSYNC_MASK|VERA_ISR_LINE_MASK|VERA_ISR_SPRCOL_MASK; //Clear any pending IRQs in the VERA core.
  VERA->IEN = VERA_IEN_VSYNC_MASK|VERA_IEN_LINE_MASK|VERA_IEN_SPRCOL_MASK; //Enable all 3 IRQ sources in the VERA core.

  printf("Starting loop... V<sprite-bank#> indicates a Vsync IRQ, L = Line IRQ, C = collision IRQ.\n");

  //The sim_main.cpp test bench checks for the presence of the trace prints below as part of the pass-fail criteria.
  while (1) {
    if (vsync_irq_fired) {
      vsync_irq_fired = 0;
      printf("V%d", VERA->CTRL);

      //After two frames, move the sprites, to create sprite collisions.
      if (frame_counter == 2) {
        printf("(Forcing sprite collision)");
        setup_sprite_attr_ram(1);
      }
    }

    if (line_irq_fired) {
      line_irq_fired = 0;
      printf("L", VERA->CTRL);
    }

    if (sprcol_irq_fired) {
      sprcol_irq_fired = 0;
      printf("C");
    }

    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
 }
}
