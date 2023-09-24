#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

#include "stdio_to_uart.h"
#include "uart.h"
#include "gpio.h"
#include "platform.h"
#include "utils.h"
#include "sdram.h"
#include "vera_hal.h"

#define VRAM_MAP_BASE (0x8000) //Relative to VERA base address.

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

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

void setup_sprite_attr_ram() {
  int i;
  unsigned v,w;

  for (i=0; i<64; i++) {
    sprite_attr_wr(i, 0x1c0, 1, 8*i, 16, 3, 0, 0);
  }

  for (i=0; i<64; i++) {
    sprite_attr_wr(i+64, 0x1000, 1, 70*i, 300, 3, 3, 3);
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
}

//_exit is executed by the picolibc exit function. 
//An implementation has to be provided to be able to user assert().

int main(void) {
  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

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
  setup_sprite_attr_ram();
  setup_palette_ram();

  //Fill VRAM map area
  for (int ii=0; ii<128*128*2; ii+=2) {
    vram_wr_byte(VRAM_MAP_BASE+ii, (unsigned char)ii&0xf);
    vram_wr_byte(VRAM_MAP_BASE+ii+1, 0);
  }

  if (read_back_err == 0)
    printf("VERA Read Back Tests successful.\n");
  else
    printf("VERA Read Back Tests failed.\n");

  unsigned int scanline;

  printf("Starting loop...\n");

  while (1) {
    scanline = vera_reg_rd(VERA_SCANLINE);

    //Switch to sprite bank 0 at scanline 479
    if (scanline == 479) {
      vera_reg_wr(VERA_CTRL, 0);
    }

    //Switch to sprite bank 1 at scanline 240
    if (scanline == 240 ) {
      vera_reg_wr(VERA_CTRL, 1);
    }

    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
    vram_wr(0,0); //Stress the bus by writing all the time.
 }
}