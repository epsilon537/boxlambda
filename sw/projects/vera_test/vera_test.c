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
//#include "libbase/memtest.h"
#include "vera_hal.h"

#define VRAM_SIZE_BYTES (128*1024)
#define VRAM_MAP_BASE (0x10000)

#define GPIO1_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

static struct uart uart0;

static struct uart uart0;
static struct gpio gpio0;
static struct gpio gpio1;

int generate_8bpp_8x8_tiles() {
  unsigned char data=0;

  //Just generate 8x8 blocks of different colors
  for (int jj=0;jj<16;jj++) {
    for (int ii=0; ii<64; ii++) {
      vram_wr_byte(jj*64+ii, (ii%8 >= 4) ? jj : 0);

      data = vram_rd_byte(jj*64+ii);

      if (data != ((ii%8 >= 4) ? jj : 0)) {
        printf("VRAM read back mismatch addr: 0x%x: 0x%x vs. 0x%x.\n\r", jj*64+ii, data, ((ii%8 >= 4) ? jj : 0));
        return -1;
      }
    }
  }

  printf("VRAM readback OK.\n\r");
  return 0;
}

void generate_8bpp_64x64_sprite() {
  for (int ii=0; ii<64*64/4; ii++) {
      vram_wr(0x1000+ii*4, 0x03030303);
    } 
}

void setup_sprite_ram() {
  int i;
  unsigned v,w;

  for (i=0; i<64; i++) {
    v = (0x1c0>>5); // addr
    v |= (1<<15); // mode: 8bpp
    v |= ((8*i)<<16); //x
    w = 16; //y
    w |= (3<<18); //z
    //width:8
    //height:8

    sprite_ram_wr(i*8, v);
    sprite_ram_wr(i*8 + 4, w);
  }

  for (i=0; i<64; i++) {
    v = (0x1000>>5); // addr
    v |= (1<<15); // mode: 8bpp
    v |= ((70*i)<<16); //x
    w = 300; //y
    w |= (3<<18); //z
    w |= (3<<28); //width:64
    w |= (3<<30); //heigth

    sprite_ram_wr(64*8 + i*8, v);
    sprite_ram_wr(64*8 + i*8 + 4, w);
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

int main(void) {
  uint32_t leds = 0xF;

  gpio_init(&gpio0, (volatile void *) PLATFORM_GPIO0_BASE);
  gpio_set_direction(&gpio0, 0x0000000F); //4 inputs, 4 outputs

  gpio_init(&gpio1, (volatile void *) PLATFORM_GPIO1_BASE);
  gpio_set_direction(&gpio1, 0x00000000); //4 inputs

  //GPIO1 bits3:0 = 0xf indicate we're running inside a simulator.
  if ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR)
    printf("This is a simulation.\n");
  else
    printf("This is not a simulation.\n");

  unsigned char read_back_val=0;

  vera_wr(VERA_DC_VIDEO, 0x71); //sprite enable, Layer 1 enable, Layer 0 enable, VGA output mode.
  read_back_val = vera_rd(VERA_DC_VIDEO);

  if (read_back_val != 0x71) {
    printf("VERA_DC_VIDEO read back incorrectly: 0x%x\n\r", read_back_val);
  }
  else {
    printf("VERA_DC_VIDEO read back OK\n\r");
  }

  vera_wr(VERA_L0_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
  vera_wr(VERA_L0_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
  vera_wr(VERA_L0_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
  vera_wr(VERA_L1_CONFIG, 0xc3); //map size 128x128, tile mode, 8bpp.
  vera_wr(VERA_L1_TILEBASE, 0x0); //tile base address 0, tile height/width 8x8.
  vera_wr(VERA_L1_MAPBASE, VRAM_MAP_BASE>>9); //Map base address 0x10000
  vera_wr(VERA_CTRL, 0); //Sprite Bank 0

  generate_8bpp_8x8_tiles();
  generate_8bpp_64x64_sprite();
  setup_sprite_ram();
  setup_palette_ram();

  //Fill VRAM map area
  for (int ii=0; ii<128*128*2; ii+=2) {
    vram_wr_byte(VRAM_MAP_BASE+ii, (unsigned char)ii&0xf);
    vram_wr_byte(VRAM_MAP_BASE+ii+1, 0);
  }

  /*sdram_init() is provided by the Litex code base.*/
  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  for (;;) {
    gpio_set_output(&gpio0, leds);
    leds ^= 0xF;

    if ((gpio_get_input(&gpio1) & 0xf) == GPIO1_SIM_INDICATOR)
      usleep(500 * 10); //Sleep less when we're running inside a simulator.
    else
      usleep(500 * 1000);
  }
}