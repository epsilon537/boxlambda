#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
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

Vera_map_t map;
Vera_tileset_t tileset0;
Vera_tileset_t tileset1;

void _vera_irq_handler(void) {
  uint32_t isr = vera_irqs_get();
  uint32_t ien = vera_irqs_enabled();

  if (isr & ien & VERA_IRQ_VSYNC) {
    ++frame_counter;
    //Switch to sprite bank 0 at vsync
    vera_spr_bank_set(VERA_SPR_BANK_0);
    vsync_irq_fired = 1;
  }

  if (isr & ien & VERA_IRQ_LINE) {
    line_irq_fired = 1;
    //Switch to sprite bank 1 at scanline 240
    vera_spr_bank_set(VERA_SPR_BANK_1);
  }

  if (isr & ien & VERA_IRQ_SPRCOL) {
    sprcol_irq_fired = 1;
  }

  //Acknowledge IRQ in the VERA core.
  vera_irqs_ack(isr&ien);

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

//Returns 0 if OK, <0 if error.
int generate_8bpp_8x8_tiles(Vera_tileset_t *tileset) {
  uint8_t data=0;
  uint8_t *tileptr;

  //Just generate 8x8 blocks of different colors
  for (int jj=0;jj<16;jj++) {
    tileptr = vera_get_tileptr(tileset, jj);

    for (int ii=0; ii<64; ii++) {
      tileptr[ii] = (ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0;

      data = vera_vram_rd_byte((uint32_t)tileset->tileset_base - VERA_VRAM_BASE + jj*64+ii);

      if (data != ((ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0)) {
        printf("VRAM read back mismatch addr: 0x%x: 0x%x vs. 0x%x.\n\r", jj*64+ii, data, ((ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0));
        return -1;
      }
    }
  }

  printf("VRAM read back OK.\n\r");
  return 0;
}

void generate_8bpp_64x64_sprite(Vera_tileset_t *tileset) {
  uint8_t *tileptr = vera_get_tileptr(tileset, 0);
  memset(tileptr, 0x03, 64*64);
}

void setup_sprite_attr_ram(Vera_tileset_t *tileset_bank_0, Vera_tileset_t *tileset_bank_1, int collide) {
  Vera_sprite_attrs_t attrs;

  //Bank 0
  attrs.addr = vera_get_tileptr(tileset_bank_0, 7);
  attrs.bpp = VERA_BPP_8;
  attrs.y = 16;
  attrs.z_depth = VERA_SPR_Z_L1;
  attrs.collision_mask = 1;
  attrs.width = VERA_TILE_SZ_8;
  attrs.height = VERA_TILE_SZ_8;

  for (int ii=0; ii<64; ii++) {
    attrs.x = collide ? 4*ii : 8*ii;
    vera_spr_attr_set(ii, &attrs);
  }

  //Bank 1
  attrs.addr = vera_get_tileptr(tileset_bank_1, 0);
  attrs.bpp = VERA_BPP_8;
  attrs.y = 300;
  attrs.z_depth = VERA_SPR_Z_L1;
  attrs.collision_mask = 1;
  attrs.width = VERA_TILE_SZ_64;
  attrs.height = VERA_TILE_SZ_64;

  for (int ii=0; ii<64; ii++) {
    attrs.x = collide ? 35*ii : 70*ii;
    vera_spr_attr_set(ii+64, &attrs);
  }
}

void setup_palette_ram(void) {
  Vera_rgb_u rgb;

  for(int ii=0; ii<256; ii++) {
    rgb.rgb.r = ((ii>>4)&3)<<2;
    rgb.rgb.g = ((ii>>2)&3)<<2;
    rgb.rgb.b = (ii&3)<<2;

    vera_palette_wr(ii, rgb);
  }
}

#ifdef __cplusplus
extern "C" {
#endif
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

#ifdef __cplusplus
}
#endif

int main(void) {
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs

  vera_hal_init();

  uint32_t read_back_val=0;
  uint32_t read_back_err=0;

  printf("Setting up VRAM...\n");

  bool res = vera_map_create(0, VERA_MAP_SZ_128, VERA_MAP_SZ_128, VERA_MAP_TYPE_TILE , &map);
  assert(res);

  //Fill VRAM map area
  Vera_map_entry_u entry;
  entry.UINT16=0;

  for (int ii=0; ii<128; ii++) {
    for (int jj=0; jj<128; jj++) {
      entry.tile.tile = (jj*2)&0xf;
      vera_map_write(&map, jj, ii, entry);
    }
  }

  res = vera_tileset_create(0, VERA_TILE_SZ_8, VERA_TILE_SZ_8, VERA_BPP_8, 256, &tileset0);
  assert(res);

  if (generate_8bpp_8x8_tiles(&tileset0) < 0)
    read_back_err = 1;

  if (read_back_err == 0) {
    printf("VERA Read Back Tests successful.\n");
  }
  else {
    printf("VERA Read Back Tests failed.\n");
  }

  res = vera_tileset_create(0, VERA_TILE_SZ_64, VERA_TILE_SZ_64, VERA_BPP_8, 1, &tileset1);
  assert(res);

  generate_8bpp_64x64_sprite(&tileset1);

  //Configure the layers
  vera_layer_hscroll_set(VERA_L0, 4);
  vera_layer_vscroll_set(VERA_L0, 4);
  vera_layer_map_props_set(VERA_L0, &map);
  vera_layer_mapbase_set(VERA_L0, &map);
  vera_layer_tilemode_set(VERA_L0, &tileset0);
  vera_layer_tilebase_set(VERA_L0, &tileset0);

  vera_layer_hscroll_set(VERA_L1, 4);
  vera_layer_vscroll_set(VERA_L1, 4);
  vera_layer_map_props_set(VERA_L1, &map);
  vera_layer_mapbase_set(VERA_L1, &map);
  vera_layer_tilemode_set(VERA_L1, &tileset0);
  vera_layer_tilebase_set(VERA_L1, &tileset0);

  setup_sprite_attr_ram(&tileset0, &tileset1, 0);
  setup_palette_ram();

  vera_irqline_set(240);

  printf("Enabling display and layers...\n");

  vera_spr_bank_set(VERA_SPR_BANK_0);
  vera_hscale_set(92);
  vera_vscale_set(92);
  vera_sprites_enable(true);
  vera_layer_enable(VERA_L0, true);
  vera_layer_enable(VERA_L1, true);
  vera_display_enable(true);

  uint32_t dc_video_reg = 0x71; //sprite enable, Layer 1 enable, Layer 0 enable, VGA output mode.
  read_back_val = VERA->DC_VIDEO;

  if (read_back_val != dc_video_reg) {
    printf("VERA_DC_VIDEO read back incorrectly: 0x%x, expected 0x%x\n\r", read_back_val, dc_video_reg);
    read_back_err = 1;
  }
  else {
    printf("VERA_DC_VIDEO read back OK\n\r");
  }

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable global IRQ line in the CPU
  enable_irq(IRQ_ID_VERA); //Enable the VERA IRQ in the CPU
  vera_irqs_ack(VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL); //Clear any pending IRQs in the VERA core.
  vera_irqs_enable(VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL); //Enable all 3 IRQ sources in the VERA core.

  printf("Starting loop... V<sprite-bank#> indicates a Vsync IRQ, L<sprite-bank#> = Line IRQ, XXX = collision IRQ.\n");

  //The sim_main.cpp test bench checks for the presence of the trace prints below as part of the pass-fail criteria.
  while (1) {
    if (vsync_irq_fired) {
      vsync_irq_fired = 0;
      printf("V%d", vera_spr_bank_get());

      //After two frames, move the sprites, to create sprite collisions.
      if (frame_counter == 2) {
        printf("(Forcing sprite collision)");
        setup_sprite_attr_ram(&tileset0, &tileset1, 1);
      }
    }

    if (line_irq_fired) {
      line_irq_fired = 0;
      printf("L%d", vera_spr_bank_get());
    }

    if (sprcol_irq_fired) {
      sprcol_irq_fired = 0;
      printf("XXX");
    }

    vera_vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera_vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera_vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera_vram_wr_word(0,0); //Stress the bus by writing all the time.
  }
}
