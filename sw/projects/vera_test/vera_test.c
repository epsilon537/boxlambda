#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "sdram.h"
#include "vera_hal.h"
#include "interrupts.h"

#define VRAM_MAP_BASE (0x8000) //Relative to VERA base address.

static struct uart uart0;
static struct gpio gpio;

volatile unsigned frame_counter = 0;
volatile unsigned vsync_irq_fired=0;
volatile unsigned line_irq_fired=0;
volatile unsigned sprcol_irq_fired=0;

void _vera_irq_handler(void) {
  unsigned isr = vera_reg_rd(VERA_ISR);
  unsigned ien = vera_reg_rd(VERA_IEN);

  if (isr & ien & VERA_IRQ_MSK_VSYNC) {
    ++frame_counter;
    //Switch to sprite bank 0 at vsync
    vera_reg_wr(VERA_CTRL, 0);
    vsync_irq_fired = 1;
  }

  if (isr & ien & VERA_IRQ_MSK_LINE) {
    line_irq_fired = 1;
    //Switch to sprite bank 1 at scanline 240
    vera_reg_wr(VERA_CTRL, 1);
  }

  if (isr & ien & VERA_IRQ_MSK_SPRCOL) {
    sprcol_irq_fired = 1;
  }

  //Acknowledge IRQ in the VERA core.
  vera_reg_wr(VERA_ISR, isr&ien);
}

//Returns 0 if OK, <0 if error.
int generate_8bpp_8x8_tiles() {
  unsigned char data=0;

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

void generate_8bpp_64x64_sprite(unsigned vram_addr) {
  for (int ii=0; ii<64*64/4; ii++) {
      vram_wr(vram_addr+ii*4, 0x03030303);
    }
}

void setup_sprite_attr_ram(int collide) {
  int i;
  unsigned v,w;

  for (i=0; i<64; i++) {
    sprite_attr_wr(i, 0x1c0, 1, collide ? 4*i : 8*i, 16, 3, 1, 0, 0);
  }

  for (i=0; i<64; i++) {
    sprite_attr_wr(i+64, 0x1000, 1, collide ? 35*i : 70*i, 300, 3, 1, 3, 3);
  }
}

void setup_palette_ram(void) {
  for (unsigned ii=0; ii<256; ii++) {
    palette_ram_wr(ii, ((ii>>4)&3)<<2, ((ii>>2)&3)<<2, (ii&3)<<2);
  }
}

//_init is executed by picolibc startup code before main().
void _init(void) {
  //Set up UART and tie stdio to it.
  uart_init(&uart0, (volatile void *) PLATFORM_UART_BASE);
  uart_set_baudrate(&uart0, 115200, PLATFORM_CLK_FREQ);
  set_stdio_to_uart(&uart0);
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
  gpio_init(&gpio, (volatile void *)GPIO_BASE);
  gpio_set_direction(&gpio, 0x0000000F); //4 inputs, 4 outputs

  unsigned read_back_val=0;
  unsigned read_back_err=0;

  printf("Setting up VERA registers...\n");

  unsigned dc_video_reg = 0x71; //sprite enable, Layer 1 enable, Layer 0 enable, VGA output mode.
  vera_reg_wr(VERA_DC_VIDEO, dc_video_reg);
  read_back_val = vera_reg_rd(VERA_DC_VIDEO);

  if (read_back_val != dc_video_reg) {
    printf("VERA_DC_VIDEO read back incorrectly: 0x%x, expected 0x%x\n\r", read_back_val, dc_video_reg);
    read_back_err = 1;
  }
  else {
    printf("VERA_DC_VIDEO read back OK\n\r");
  }

  vera_reg_wr(VERA_L0_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
  vera_reg_wr(VERA_L0_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
  vera_reg_wr(VERA_L0_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
  vera_reg_wr(VERA_L0_HSCROLL, 4);
  vera_reg_wr(VERA_L0_VSCROLL, 4);

  vera_reg_wr(VERA_L1_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
  vera_reg_wr(VERA_L1_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
  vera_reg_wr(VERA_L1_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
  vera_reg_wr(VERA_L1_HSCROLL, 4);
  vera_reg_wr(VERA_L1_VSCROLL, 4);
  vera_reg_wr(VERA_CTRL, 0); //Sprite Bank 0

  vera_reg_wr(VERA_DC_HSCALE, 92);
  vera_reg_wr(VERA_DC_VSCALE, 92);

  printf("Setting up VRAM...\n");

  if (generate_8bpp_8x8_tiles() < 0)
    read_back_err = 1;

  generate_8bpp_64x64_sprite(/*vram_addr=*/0x1000);
  setup_sprite_attr_ram(0);
  setup_palette_ram();

  //Fill VRAM map area
  for (int ii=0; ii<128*128*2; ii+=2) {
    vram_wr_byte(VRAM_MAP_BASE+ii, (unsigned char)ii&0xf);
    vram_wr_byte(VRAM_MAP_BASE+ii+1, 0);
  }

  if (read_back_err == 0) {
    printf("VERA Read Back Tests successful.\n");
  }
  else {
    printf("VERA Read Back Tests failed.\n");
  }

  vera_reg_wr(VERA_IRQ_LINE, 240);

  printf("Enabling IRQs\n");
  enable_global_irq(); //Enable global IRQ line in the CPU
  enable_irq(IRQ_ID_VERA); //Enable the VERA IRQ in the CPU
  vera_reg_wr(VERA_ISR, VERA_IRQ_MSK_VSYNC|VERA_IRQ_MSK_LINE|VERA_IRQ_MSK_SPRCOL); //Clear any pending IRQs in the VERA core.
  vera_reg_wr(VERA_IEN, VERA_IRQ_MSK_VSYNC|VERA_IRQ_MSK_LINE|VERA_IRQ_MSK_SPRCOL); //Enable all 3 IRQ sources in the VERA core.

  printf("Starting loop... V<sprite-bank#> indicates a Vsync IRQ, L = Line IRQ, C = collision IRQ.\n");

  //The sim_main.cpp test bench checks for the presence of the trace prints below as part of the pass-fail criteria.
  while (1) {
    if (vsync_irq_fired) {
      vsync_irq_fired = 0;
      printf("V%d", vera_reg_rd(VERA_CTRL));

      //After two frames, move the sprites, to create sprite collisions.
      if (frame_counter == 2) {
        printf("(Forcing sprite collision)");
        setup_sprite_attr_ram(1);
      }
    }

    if (line_irq_fired) {
      line_irq_fired = 0;
      printf("L", vera_reg_rd(VERA_CTRL));
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
