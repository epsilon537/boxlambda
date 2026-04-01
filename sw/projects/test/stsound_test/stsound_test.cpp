#include "YmMusicBL.h"
#include "mcycle.h"
#include "uart.h"
#include "gpio.h"
#include "ff.h"
#include <stdio.h>
#include <string.h>
#include "ym2149_regs.h"
#include "sdram.h"

//This test program loads a .ym music file from the SD card and plays it back on one of the
//two YM2149 PSGs using the STsound library.

#define YM2149_PSG_0 (YM2149_BASE_ADDR+YM2149_PSG0_CHA_TONE_PERIOD_FINE_ADDR)
#define YM2149_PSG_1 (YM2149_BASE_ADDR+YM2149_PSG1_CHA_TONE_PERIOD_FINE_ADDR)

#define DISK_DEV_NUM "0:"
#define STR_ROOT_DIRECTORY ""

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.
const char *root_dir_name = STR_ROOT_DIRECTORY;
const char *ym_file_name = STR_ROOT_DIRECTORY "ancool1.ym";

typedef struct {
  uint32_t addr;
  uint32_t val;
} AddrVal_t;

uint32_t mval = 10;
uint32_t bass = 25;
uint32_t treble = 128;

#ifdef __cplusplus
extern "C"
{
#endif

  //_init is executed by picolibc startup code before main().
  void _init(void)
  {
    mcycle_start();
  }

  //_exit is executed by the picolibc exit function.
  // An implementation has to be provided to be able to user assert().
  void _exit(int status)
  {
    while (1);
  }

#ifdef __cplusplus
}
#endif

FILINFO Finfo;
#if USE_LFN
char Lfname[512];
#endif

//This function lists the given path's directory contents.
static FRESULT scan_files(
  const char *path /* Pointer to the path name working buffer */
)
{
  DIR dirs;
  FRESULT res;
  BYTE i;
  char *fn;

  if ((res = f_opendir(&dirs, path)) == FR_OK)
  {
    i = strlen(path);
    while (((res = f_readdir(&dirs, &Finfo)) == FR_OK) && Finfo.fname[0])
    {
      if (Finfo.fname[0] == '.')
        continue;
#if USE_LFN
      fn = *Finfo.lfname ? Finfo.lfname : Finfo.fname;
#else
      fn = Finfo.fname;
#endif
      printf("%s/%s\n", path, fn);
    }
  }

  return res;
}

static inline void ym2149_sys_reg_wr(uint32_t reg_offset, uint32_t val)
{
  *(uint32_t volatile *)(YM2149_BASE_ADDR + reg_offset) = val;
}

int main(void)
{
  FRESULT res;
  static DIR dirs;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  if (sdram_init()) {
    printf("SDRAM init OK.\n");
  }
  else {
    printf("SDRAM init failed!\n");
    while(1);
  }

  //Program the audio mixer registers
  static AddrVal_t addr_vals[]  = {
    {YM2149_FILTER_MIXER_VOLA_ADDR, 50},
    {YM2149_FILTER_MIXER_VOLB_ADDR, 50},
    {YM2149_FILTER_MIXER_VOLC_ADDR, 50},
    {YM2149_FILTER_MIXER_VOLD_ADDR, 50},
    {YM2149_FILTER_MIXER_VOLE_ADDR, 50},
    {YM2149_FILTER_MIXER_VOLF_ADDR, 50},
    {YM2149_FILTER_MIXER_MVOL_ADDR, mval},
    {YM2149_FILTER_MIXER_INV_ADDR, 0},
    {YM2149_FILTER_MIXER_BASS_ADDR, bass},
    {YM2149_FILTER_MIXER_TREBLE_ADDR, treble} } ;

  for (int ii=0; ii<(sizeof(addr_vals)/sizeof(AddrVal_t)); ii++) {
    ym2149_sys_reg_wr(addr_vals[ii].addr, addr_vals[ii].val);
  }

  /* Declare these as static to avoid stack usage.
   * They each contain an array of maximum sector size.
   */
  static FATFS fs;

  printf("Mounting...\n");
  /* Clear file system object */
  memset(&fs, 0, sizeof(FATFS));
  res = f_mount(&fs, "", 1);
  if (res != FR_OK)
  {
    printf("FatFS mount error! %d\n", res);
    return -1;
  }

  printf("Listing directory contents...\n");
  scan_files(root_dir_name);

  //Instantiate two PSGs. Based on switch 3 setting, the music will play on one or the other.
  static CYmMusic cyMusic_psg_0((volatile ymint *)YM2149_PSG_0);
  static CYmMusic cyMusic_psg_1((volatile ymint *)YM2149_PSG_1);
  CYmMusic* cyMusicp = 0;

  if (gpio_get_input() & 0x80)
  {
    printf("Switching to PSG_0\n");
    cyMusicp = &cyMusic_psg_0;
  }
  else
  {
    printf("Switching to PSG_1\n");
    cyMusicp = &cyMusic_psg_1;
  }

  printf("Loading YM file: %s ...\n", ym_file_name);
  if (!cyMusicp->load(ym_file_name))
  {
    printf("CyMusic load failed!\n");
    return -1;
  }

  cyMusicp->setLoopMode(true);
  printf("Starting playback...\n");

  cyMusicp->play();

  uint32_t prevTimeClocks = mcycle_get32();
  uint32_t curTimeClocks;

  while (1)
  {
    curTimeClocks = mcycle_get32();

    //Every 20ms...
    if (cc2us(curTimeClocks - prevTimeClocks) >= 20000)
    {
      prevTimeClocks = curTimeClocks;
      cyMusicp->player(); //'tick' the music player.

      //Set switch 0, 1 or 2 to select volume, bass or treble control.
        //Then press buttons 0 or 1 to increase/decrease.
      if (gpio_get_input() & 0x10)
      {
        if (gpio_get_input() & 0x0100)
        {
          if (mval < 255)
            ++mval;

          YM2149->FILTER_MIXER_MVOL = mval;
          printf("mval: %d\n", mval);
        }

        if (gpio_get_input() & 0x0200)
        {
          if (mval > 0)
            --mval;

          YM2149->FILTER_MIXER_MVOL = mval;
          printf("mval: %d\n", mval);
        }
      }

      if (gpio_get_input() & 0x20)
      {
        if (gpio_get_input() & 0x0100)
        {
          if (bass < 63)
            ++bass;

          YM2149->FILTER_MIXER_BASS = bass;
          printf("bass: %d\n", bass);
        }

        if (gpio_get_input() & 0x0200)
        {
          if (bass > 1)
            --bass;

          YM2149->FILTER_MIXER_BASS = bass;
          printf("bass: %d\n", bass);
        }
      }

      if (gpio_get_input() & 0x40)
      {
        if (gpio_get_input() & 0x0100)
        {
          if (treble < 255)
            ++treble;

          YM2149->FILTER_MIXER_TREBLE = treble;
          printf("treble: %d\n", treble);
        }

        if (gpio_get_input() & 0x0200)
        {
          if (treble > 0)
            --treble;

          YM2149->FILTER_MIXER_TREBLE = treble;
          printf("treble: %d\n", treble);
        }
      }
    }
  }

  return 0;
}
