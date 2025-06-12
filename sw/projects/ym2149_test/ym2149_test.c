#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "ym2149_sys_regs.h"

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

unsigned mval = 10;
unsigned bass = 25;
unsigned treble = 128;

//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

int main(void) {
  //Switches
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs


  printf("YM2149 test.\n");

  //Set 6 different pitches in the 6 channels
  unsigned addrs[] = {
    0,  1,  2,  3,    4, 5,  6,    7 ,  8,  9, 10, 11, 12, 13,    16  , 17,   18, 19,   20, 21, 22,   23, 24, 25, 26, 27, 28, 29, 128,129, 130,131,132,133, 134, 135, 136, 137 } ;
  unsigned vals[]  = {
    0x1c,1,0xfd, 0, 0xef, 0,  0, 0xf8 , 15,  15, 15,  1,  0, 13,   0xd5,  0, 0xbe,  0, 0xb3,  0,  0, 0xf8, 15, 15, 15,  1,  0, 13,  64, 64,  64, 64, 64, 64, mval,   0,  bass, treble } ;

  for (int ii=0; ii<(sizeof(addrs)/sizeof(addrs[0])); ii++) {
    ym2149_sys_reg_wr(addrs[ii], vals[ii]);
  }

  printf("YM2149 config complete.\n");

  while(1) {
    //Set switch 0, 1 or 2 to select volume, bass or treble control.
    //Then press buttons 0 or 1 to increase/decrease.

    if (gpio_get_input() & 0x10) {
      if (gpio_get_input() & 0x0100) {
        if (mval < 255)
          ++mval;

        ym2149_sys_reg_wr(FILTER_MIXER_MVOL_OFFSET, mval);
        printf("mval: %d\n", mval);
      }

      if (gpio_get_input() & 0x0200) {
        if (mval > 0)
          --mval;

        ym2149_sys_reg_wr(FILTER_MIXER_MVOL_OFFSET, mval);
        printf("mval: %d\n", mval);
      }
    }

    if (gpio_get_input() & 0x20) {
      if (gpio_get_input() & 0x0100) {
        if (bass < 63)
          ++bass;

        ym2149_sys_reg_wr(FILTER_MIXER_BASS_OFFSET, bass);
        printf("bass: %d\n", bass);
      }

      if (gpio_get_input() & 0x0200) {
        if (bass > 1)
          --bass;

        ym2149_sys_reg_wr(FILTER_MIXER_BASS_OFFSET, bass);
        printf("bass: %d\n", bass);
      }
    }

    if (gpio_get_input() & 0x40) {
      if (gpio_get_input() & 0x0100) {
        if (treble < 255)
          ++treble;

        ym2149_sys_reg_wr(FILTER_MIXER_TREB_OFFSET, treble);
        printf("treble: %d\n", treble);
      }

      if (gpio_get_input() & 0x0200) {
        if (treble > 0)
          --treble;

        ym2149_sys_reg_wr(FILTER_MIXER_TREB_OFFSET, treble);
        printf("treble: %d\n", treble);
      }
    }

    usleep(50000);
  }

  return 0;
}
