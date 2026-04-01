#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "gpio.h"
#include "mcycle.h"
#include "ym2149_regs.h"

#define GPIO_SIM_INDICATOR 0xf //If GPIO1 inputs have this value, this is a simulation.

typedef struct {
  uint32_t addr;
  uint32_t val;
} AddrVal_t;

uint32_t mval = 10;
uint32_t bass = 25;
uint32_t treble = 128;

//_init is executed by picolibc startup code before main().
void _init(void) {
  uart_set_baudrate(115200);
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

static inline void ym2149_sys_reg_wr(uint32_t reg_offset, uint32_t val)
{
  *(uint32_t volatile *)(YM2149_BASE_ADDR + reg_offset) = val;
}

int main(void) {
  //Switches
  gpio_init();
  gpio_set_direction(0x0000000F); //4 inputs, 4 outputs


  printf("YM2149 test.\n");

  //Set 6 different pitches in the 6 channels
  AddrVal_t addr_vals[]  = {
    {YM2149_PSG0_CHA_TONE_PERIOD_FINE_ADDR, 0x1c},
    {YM2149_PSG0_CHA_TONE_PERIOD_COARSE_ADDR, 1},
    {YM2149_PSG0_CHB_TONE_PERIOD_FINE_ADDR, 0xfd},
    {YM2149_PSG0_CHB_TONE_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG0_CHC_TONE_PERIOD_FINE_ADDR, 0xef},
    {YM2149_PSG0_CHC_TONE_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG0_NOISE_PERIOD_ADDR, 0},
    {YM2149_PSG0_DISABLE_ADDR,
      ~(uint32_t)(YM2149_PSG0_DISABLE_TONE_A_MASK|YM2149_PSG0_DISABLE_TONE_B_MASK|YM2149_PSG0_DISABLE_TONE_C_MASK)},
    {YM2149_PSG0_CHA_LVL_ADDR, 15},
    {YM2149_PSG0_CHB_LVL_ADDR, 15},
    {YM2149_PSG0_CHC_LVL_ADDR, 15},
    {YM2149_PSG0_ENV_PERIOD_FINE_ADDR, 1},
    {YM2149_PSG0_ENV_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG0_ENV_SHAPE_ADDR, 13},
    {YM2149_PSG1_CHA_TONE_PERIOD_FINE_ADDR, 0xd5},
    {YM2149_PSG1_CHA_TONE_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG1_CHB_TONE_PERIOD_FINE_ADDR, 0xbe},
    {YM2149_PSG1_CHB_TONE_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG1_CHC_TONE_PERIOD_FINE_ADDR, 0xb3},
    {YM2149_PSG1_CHC_TONE_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG1_NOISE_PERIOD_ADDR, 0},
    {YM2149_PSG1_DISABLE_ADDR,
      ~(uint32_t)(YM2149_PSG1_DISABLE_TONE_A_MASK|YM2149_PSG1_DISABLE_TONE_B_MASK|YM2149_PSG1_DISABLE_TONE_C_MASK)},
    {YM2149_PSG1_CHA_LVL_ADDR, 15},
    {YM2149_PSG1_CHB_LVL_ADDR, 15},
    {YM2149_PSG1_CHC_LVL_ADDR, 15},
    {YM2149_PSG1_ENV_PERIOD_FINE_ADDR, 1},
    {YM2149_PSG1_ENV_PERIOD_COARSE_ADDR, 0},
    {YM2149_PSG1_ENV_SHAPE_ADDR, 13},
    {YM2149_FILTER_MIXER_VOLA_ADDR, 64},
    {YM2149_FILTER_MIXER_VOLB_ADDR, 64},
    {YM2149_FILTER_MIXER_VOLC_ADDR, 64},
    {YM2149_FILTER_MIXER_VOLD_ADDR, 64},
    {YM2149_FILTER_MIXER_VOLE_ADDR, 64},
    {YM2149_FILTER_MIXER_VOLF_ADDR, 64},
    {YM2149_FILTER_MIXER_MVOL_ADDR, mval},
    {YM2149_FILTER_MIXER_INV_ADDR, 0},
    {YM2149_FILTER_MIXER_BASS_ADDR, bass},
    {YM2149_FILTER_MIXER_TREBLE_ADDR, treble} } ;

  for (int ii=0; ii<(sizeof(addr_vals)/sizeof(AddrVal_t)); ii++) {
    ym2149_sys_reg_wr(addr_vals[ii].addr, addr_vals[ii].val);
  }

  printf("YM2149 config complete.\n");

  while(1) {
    //Set switch 0, 1 or 2 to select volume, bass or treble control.
    //Then press buttons 0 or 1 to increase/decrease.

    if (gpio_get_input() & 0x10) {
      if (gpio_get_input() & 0x0100) {
        if (mval < 255)
          ++mval;

        YM2149->FILTER_MIXER_MVOL = mval;
        printf("mval: %d\n", mval);
      }

      if (gpio_get_input() & 0x0200) {
        if (mval > 0)
          --mval;

        YM2149->FILTER_MIXER_MVOL = mval;
        printf("mval: %d\n", mval);
      }
    }

    if (gpio_get_input() & 0x20) {
      if (gpio_get_input() & 0x0100) {
        if (bass < 63)
          ++bass;

        YM2149->FILTER_MIXER_BASS = bass;
        printf("bass: %d\n", bass);
      }

      if (gpio_get_input() & 0x0200) {
        if (bass > 1)
          --bass;

        YM2149->FILTER_MIXER_BASS = bass;
        printf("bass: %d\n", bass);
      }
    }

    if (gpio_get_input() & 0x40) {
      if (gpio_get_input() & 0x0100) {
        if (treble < 255)
          ++treble;

        YM2149->FILTER_MIXER_TREBLE = treble;
        printf("treble: %d\n", treble);
      }

      if (gpio_get_input() & 0x0200) {
        if (treble > 0)
          --treble;

        YM2149->FILTER_MIXER_TREBLE = treble;
        printf("treble: %d\n", treble);
      }
    }

    usleep(50000);
  }

  return 0;
}
