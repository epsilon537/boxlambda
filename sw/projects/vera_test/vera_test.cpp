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

Vera_map *map;
Vera_tileset *tileset0;
Vera_tileset *tileset1;

void _vera_irq_handler(void) {
  uint32_t isr = vera.irqs_get();
  uint32_t ien = vera.irqs_enabled();

  if (isr & ien & VERA_IRQ_VSYNC) {
    ++frame_counter;
    //Switch to sprite bank 0 at vsync
    vera.sprite_bank_set(VERA_SPR_BANK_0);
    vsync_irq_fired = 1;
  }

  if (isr & ien & VERA_IRQ_LINE) {
    line_irq_fired = 1;
    //Switch to sprite bank 1 at scanline 240
    vera.sprite_bank_set(VERA_SPR_BANK_1);
  }

  if (isr & ien & VERA_IRQ_SPRCOL) {
    sprcol_irq_fired = 1;
  }

  //Acknowledge IRQ in the VERA core.
  vera.irqs_ack(isr&ien);

  //Return from interrupt
  __asm__ volatile (
      "mret \n"
  );
}

//Returns 0 if OK, <0 if error.
int generate_8bpp_8x8_tiles(Vera_tileset *tileset) {
  uint8_t data=0;
  uint8_t *tileptr;

  assert(tileset);

  //For 16 tiles generate 8x8 blocks of different colors
  for (int jj=0;jj<16;jj++) {
    tileptr = tileset->tile(jj);

    for (int ii=0; ii<64; ii++) {
      tileptr[ii] = (ii<32) ? ((ii%8 < 4) ? jj : 0) : 0;

      data = vera.vram_rd_byte((uint32_t)tileset->tileset_base() - VERA_VRAM_BASE + jj*64+ii);

      if (data != ((ii<32) ? ((ii%8 < 4) ? jj : 0) : 0)) {
        printf("VRAM read back mismatch addr: 0x%x: 0x%x vs. 0x%x.\n\r", jj*64+ii, data, ((ii<32) ? ((ii%8 >= 4) ? jj : 0) : 0));
        return -1;
      }
    }
  }

  printf("VRAM read back OK.\n\r");
  return 0;
}

//Generate the pixel data for 1 sprite.
void generate_8bpp_64x64_sprite(Vera_tileset *tileset) {
  assert(tileset);

  uint8_t *tileptr = tileset->tile(0);
  memset(tileptr, 0x03, 64*64);
}

void setup_sprite_attr_ram(int collide) {
  //Bank 0
  for (int ii=0; ii<64; ii++) {
    Vera_sprite& sprite = vera.sprite[ii];
    sprite.tile_set(0, 7);
    sprite.x_set(collide ? 4*ii : 8*ii);
    sprite.y_set(16);
    sprite.collision_mask_set(1);
    sprite.z_depth_set(VERA_SPR_Z_L1);
  }

  //Bank 1
  for (int ii=0; ii<64; ii++) {
    Vera_sprite& sprite = vera.sprite[ii+64];
    sprite.tile_set(1, 0);
    sprite.x_set(collide ? 35*ii : 70*ii);
    sprite.y_set(300);
    sprite.collision_mask_set(1);
    sprite.z_depth_set(VERA_SPR_Z_L1);
  }
}

void setup_palette_ram(void) {
  Vera_rgb_t rgb;

  for(int ii=1; ii<256; ii++) {
    rgb.r = ((ii>>4)&3)<<2;
    rgb.g = ((ii>>2)&3)<<2;
    rgb.b = (ii&3)<<2;

    vera.palette.write(ii, rgb);
  }

  //Make background white.
  vera.palette.write(0, 0xfff);
}

void check_line_capture(void) {
  if (!vera.line_capture_enabled()) {
    printf("P");
    uint32_t irq_line = vera.irqline_get();
    uint32_t row = irq_line/8;
    uint32_t row_offset = irq_line%8;
    uint32_t col;
    uint32_t col_offset;
    Vera_tilemap_entry_t tilemap_entry;
    uint8_t pal_idx;
    Vera_rgb_u rgb_expected;
    Vera_rgb_u rgb_actual;
    bool checkok = true;

    for (int ii=0; ii<640; ii++) {
      col = ii/8;
      col_offset = ii%8;

      tilemap_entry = vera.map[0].read_tile(col, row);
      pal_idx = vera.tileset[0].pixel_get(tilemap_entry.tile, col_offset, row_offset);
      rgb_expected.rgb = vera.palette.read_rgb(pal_idx);
      rgb_actual.rgb = vera.line_capture_read_pixel(ii);
      if (rgb_expected.UINT16 != rgb_actual.UINT16) {
        printf("col: %d, row: %d, col_offset: %d, row_offset: %d\n", col, row, col_offset, row_offset);
        printf("tile: %d, pal_idx: %d\n", tilemap_entry.tile, pal_idx);
        printf("[%d] expected: 0x%03X actual: 0x%03X\n", ii, rgb_expected.UINT16, rgb_actual.UINT16);
        checkok = false;
      }
    }

    if (checkok) {
      printf("Line capture check OK.\n");
    }
    else {
      printf("Line capture check failed.\n");
      while (1);
    }
  }
  else {
    printf("ERROR: Line capture did not trigger!\n");
    while (1); //Just sit here and spin.
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

  uint32_t read_back_val=0;
  uint32_t read_back_err=0;

  printf("Setting up VRAM...\n");

  bool res;

  map = &vera.map[0];
  res = map->init(VERA_MAP_SZ_128, VERA_MAP_SZ_128, VERA_MAP_TYPE_TILE);

  assert(res);

  //Fill VRAM map area
  uint16_t entry;

  for (int ii=0; ii<128; ii++) {
    for (int jj=0; jj<128; jj++) {
      map->write(jj, ii, ((jj+2)*2)&0xf);
    }
  }

  tileset0 = &vera.tileset[0];
  res = tileset0->init(VERA_TILE_SZ_8, VERA_TILE_SZ_8, VERA_BPP_8, 256);

  assert(res);

  if (generate_8bpp_8x8_tiles(tileset0) < 0)
    read_back_err = 1;

  if (read_back_err == 0) {
    printf("VERA Read Back Tests successful.\n");
  }
  else {
    printf("VERA Read Back Tests failed.\n");
  }

  tileset1 = &vera.tileset[1];
  res = tileset1->init(VERA_TILE_SZ_64, VERA_TILE_SZ_64, VERA_BPP_8, 1);

  assert(res);

  generate_8bpp_64x64_sprite(tileset1);

  //Configure the layers
  vera.layer[0].hscroll_set(0);
  assert(vera.layer[0].hscroll_get() == 0);

  vera.layer[0].vscroll_set(0);
  vera.layer[0].map_set(0);
  vera.layer[0].tileset_set(0);

  vera.layer[1].hscroll_set(0);
  assert(vera.layer[0].hscroll_get() == 0);

  vera.layer[1].vscroll_set(0);
  vera.layer[1].map_set(0);
  vera.layer[1].tileset_set(0);

  setup_sprite_attr_ram(0);
  setup_palette_ram();

  vera.irqline_set(240);

  printf("Enabling display and layers...\n");

  Vera_screen_boundaries_t screen_boundaries = {0, 640, 0, 480};

  vera.screen_boundaries_set(&screen_boundaries);

  vera.sprite_bank_set(VERA_SPR_BANK_0);
  vera.hscale_set(128);
  vera.vscale_set(128);
  vera.sprites_enable(true);
  vera.layer[0].enable(true);
  vera.layer[1].enable(true);
  vera.display_enable(true);

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
  vera.irqs_ack(VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL); //Clear any pending IRQs in the VERA core.
  vera.irqs_enable(VERA_IRQ_VSYNC|VERA_IRQ_LINE|VERA_IRQ_SPRCOL); //Enable all 3 IRQ sources in the VERA core.

  printf("Starting loop... V<sprite-bank#> indicates a Vsync IRQ, L<sprite-bank#> = Line IRQ, XXX = collision IRQ.\n");
  printf("P = Line captured.\n");

  //The sim_main.cpp test bench checks for the presence of the trace prints below as part of the pass-fail criteria.
  while (1) {
    if (vsync_irq_fired) {
      vsync_irq_fired = 0;
      printf("V%d", vera.sprite_bank_get());

      //On frame 1, request VGA line capture (on the IRQ line).
      if (frame_counter == 1) {
        vera.line_capture_enable(true);

        assert(vera.line_capture_enabled());
      }

      //After two frames, move the sprites, to create sprite collisions.
      if (frame_counter == 2) {
        //By now the VGA line should have been captured.
        check_line_capture();

        printf("(Forcing sprite collision)");
        setup_sprite_attr_ram(1);
      }
    }

    if (line_irq_fired) {
      line_irq_fired = 0;
      printf("L%d", vera.sprite_bank_get());
    }

    if (sprcol_irq_fired) {
      sprcol_irq_fired = 0;
      printf("XXX");
    }

    vera.vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera.vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera.vram_wr_word(0,0); //Stress the bus by writing all the time.
    vera.vram_wr_word(0,0); //Stress the bus by writing all the time.
  }
}
