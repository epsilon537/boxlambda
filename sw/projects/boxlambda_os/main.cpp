#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "fatal.h"

#include "gpio.h"
#include "forth.h"
#include "fs_ffi.h"
#include "stdio_redirect_ffi.h"
#include "diskio_ram.h"
#include "ff.h"
#include "uart.h"
#ifdef FORTH_CORE_TEST
#include "forth_core_test.h"
#endif /*FORTH_CORE_TEST*/

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.

#ifdef __cplusplus
extern "C" {
#endif
//_init is executed by picolibc startup code before main().
void _init(void) {
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to use assert().
void	_exit (int status) {
	while (1);
}

//The RAM FS image location.
extern char __fs_image_start[];
extern char __fs_image_size[];

// The file system objects
#define NUM_VOLS 2
#define SD_VOL 0
#define RAM_VOL 1
Fs_Volume_t volumes[NUM_VOLS];

const char *sd_vol_name = "/sd";
const char *ram_vol_name = "/ram";
const char *sd_boot_path = "/sd/forth";
const char *ram_boot_path = "/ram/forth";

#ifdef __cplusplus
}
#endif

// Attempts to mount SD and/or RAM disk and detect boot dir. Prompts user and retries if needed. Returns boot path. Operates on volumes array.
const char *mount_get_boot_dir() {
  printf("Mounting file system...\n");

  /* Initialize the volume objects. */
  memset(&(volumes[SD_VOL].vol), 0, sizeof(FATFS));
  memset(&(volumes[RAM_VOL].vol), 0, sizeof(FATFS));
  volumes[SD_VOL].name = sd_vol_name;
  volumes[RAM_VOL].name = ram_vol_name;

  const char *boot_path = 0;

  bool fs_mounted = false;

  bool fr;
  FILINFO fno;

  while (true) {
    FRESULT res = f_mount(&(volumes[SD_VOL].vol), volumes[SD_VOL].name, 1);
    if (res != FR_OK)
    {
      printf("No SD FatFS detected.\n");
    }
    else {
      printf("SD FatFS mounted.\n");
      fr = f_stat(sd_boot_path, &fno);
      if ((fr == 0) && (fno.fattrib & AM_DIR)) {
        printf("Boot path %s found;\n", sd_boot_path);
        boot_path = sd_boot_path;
      }

      fs_mounted = true;
    }

    disk_ram_init((unsigned char *)__fs_image_start, (unsigned long)__fs_image_size);

    res = f_mount(&(volumes[RAM_VOL].vol), volumes[RAM_VOL].name, 1);
    if (res != FR_OK)
    {
      printf("No RAM FatFS detected.\n");
    }
    else {
      printf("RAM FatFS mounted.\n");
      fr = f_stat(ram_boot_path, &fno);
      if ((fr == 0) && (fno.fattrib & AM_DIR)) {
        printf("Boot path %s found;\n", ram_boot_path);
        boot_path = ram_boot_path; //RAM boot takes precedence over SD boot.
      }

      fs_mounted = true;
    }

    if (fs_mounted && boot_path) break;

    if (!fs_mounted) {
      printf("No Filesystem found. Please insert FAT formatted SD card or upload RAM disk. Then press enter to continue.\n");
    }
    else {
      printf("No forth/ boot directory found. Please insert SD card or upload RAM disk with boot directory. Then press enter to continue.\n");
    }

    getchar();
  }

  return boot_path;
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  printf("Initializing Forth...\n");

  forth_core_init();

  printf("Forth core init complete.\n");

  const char *boot_path = mount_get_boot_dir();

  printf("Booting from %s.\n", boot_path);

  FRESULT fr = f_chdir(boot_path);
  assert(fr == 0);

  forth_eval_file_or_die("early.fs", /*verbose=*/ false);

  printf("Redirecting stdio to Forth...\n");
  stdio_redirect_ffi_init();

  forth_eval_file_or_die("except.fs", /*verbose=*/ false);
  forth_eval_file_or_die("lambda.fs", /*verbose=*/ false);
  forth_eval_file_or_die("struct.fs", /*verbose=*/ false);
  forth_eval_file_or_die("heap.fs", /*verbose=*/ false);
  forth_eval_file_or_die("pool.fs", /*verbose=*/ false);
  forth_eval_file_or_die("temp-alloc.fs", /*verbose=*/ false);
  forth_eval_file_or_die("istr.fs", /*verbose=*/ false);
  forth_eval_file_or_die("escstr.fs", /*verbose=*/ false);
  forth_eval_file_or_die("tonumber.fs", /*verbose=*/ false);
  forth_eval_file_or_die("printf.fs", /*verbose=*/ false);
  forth_eval_file_or_die("cstr.fs", /*verbose=*/ false);

  printf("Initializing Forth Filesystem FFI...\n");

  fs_ffi_init(volumes, NUM_VOLS);

  forth_eval_file_or_die("fs.fs", /*verbose=*/ false);
  forth_eval_file_or_die("fs_redirect.fs", /*verbose=*/ false);
  forth_eval_file_or_die("shell.fs", /*verbose=*/ false);

  fr = f_chdir(".."); // cd to root of boot volume
  assert(fr == 0);

#ifdef FORTH_CORE_TEST
  forth_core_test();
  forth_eval("create FORTH_CORE_TEST");
#endif /*FORTH_CORE_TEST*/

  forth_eval("include forth/init.fs");

  die("\nForth REPL exited.\n");
}

