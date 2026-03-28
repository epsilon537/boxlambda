//
// This is the BoxLambda kernel (BoxKern) top-level/main module.
//
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>
#include "fatal.h"
#include "inout.h"

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

//The RAM disk image location variables provide by linker.
extern char __fs_image_start[];
extern char __fs_image_size[];

// The file system objects
#define NUM_VOLS 2
#define SD_VOL 0
#define RAM_VOL 1
Fs_Volume_t volumes[NUM_VOLS];

const char *sd_vol_name = "sd0:";
const char *ram_vol_name = "ram:";

#ifdef __cplusplus
}
#endif

// Attempts to mount given volume and checks if boot path (/forth/) exists.
// Returns true if boot path is found.
bool mount_vol(Fs_Volume_t& vol) {
  bool boot_path_found = false;
  static char boot_path[] = "XXX:forth";

  assert(vol.name);

  FRESULT res = f_mount(&(vol.vol), vol.name, 1);
  if (res == FR_OK) {
    printf("%s mounted.\n", vol.name);

    memcpy(boot_path, vol.name, 4);
    FILINFO fno;
    res = f_stat(boot_path, &fno);
    if ((res == FR_OK) && (fno.fattrib & AM_DIR)) {
      printf("Boot path %s found.\n", boot_path);
      boot_path_found = true;
    }
    else {
      printf("No boot path found on %s.\n", vol.name);
    }
  }
  else {
    printf("No FatFS detected on %s.\n", vol.name);
  }

  return boot_path_found;
}

// Attempts to mount sd0: and/or ram: volumes and detect boot dir (/forth/).
// If both volumes have a boot directory, ram: takes precedence.
// Prompts user and retries if needed. Returns boot volume. Operates on volumes[] array.
// Returns boot volume name.
const char *mount_vols() {
  printf("Mounting file system...\n");

  /* Initialize the volume objects. */
  memset(&(volumes[SD_VOL].vol), 0, sizeof(FATFS));
  memset(&(volumes[RAM_VOL].vol), 0, sizeof(FATFS));
  volumes[SD_VOL].name = sd_vol_name;
  volumes[RAM_VOL].name = ram_vol_name;

  const char *boot_vol = 0;

  while (true) {
    if (mount_vol(volumes[SD_VOL]))
      boot_vol=volumes[SD_VOL].name;
    if (mount_vol(volumes[RAM_VOL]))
      boot_vol=volumes[RAM_VOL].name; //ram: overrules sd0 as boot volume.

    if (boot_vol) {
      FRESULT res = f_chdrive(boot_vol);
      assert(res == FR_OK);
      break;
    }

    printf("No bootable filesystem found. Please insert FAT formatted SD card or upload RAM disk. Then press enter to continue.\n");

    getchar();
  }

  return boot_vol;
}

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  printf("Initializing Forth...\n");

  forth_core_init();

  printf("Forth core init complete.\n");

  disk_ram_init((unsigned char *)__fs_image_start, (unsigned long)__fs_image_size);

  const char *boot_vol = mount_vols();

  printf("Booting from volume %s.\n", boot_vol);

  // Early.fs has to go first. It defines words used by the FFI modules below.
  forth_eval_file_or_die("forth/early.fs", /*verbose=*/ false);

  // Up to this point stdio is handled by bootstrap/stdio_stream.[ch]
  // We now switch it over to Forth.
  printf("Redirecting stdio to Forth...\n");
  stdio_redirect_ffi_init();

  printf("Initializing Forth Filesystem FFI...\n");
  fs_ffi_init(volumes, NUM_VOLS);

  // The order is important. They build up a stack, with shell.fs on top.
  forth_eval_file_or_die("forth/except.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/lambda.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/struct.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/heap.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/pool.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/temp-alloc.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/istr.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/escstr.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/tonumber.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/printf.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/cstr.fs", /*verbose=*/ false);

  forth_eval_file_or_die("forth/fs.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/fs_redirect.fs", /*verbose=*/ false);
  forth_eval_file_or_die("forth/shell.fs", /*verbose=*/ false);

// Set when building boxkerntest.
#ifdef FORTH_CORE_TEST
  forth_core_test(); // Execute the Forth <-> C FFI testsuite.
  forth_eval("create FORTH_CORE_TEST"); // Used by an [ifdef] block in init.fs used to execute the Forth testsuite.
#endif /*FORTH_CORE_TEST*/

  // We now transfer control to init.fs. Control does not return unless the user invokes 'bye'.
  forth_eval("include forth/init.fs");

  die("\nForth REPL exited.\n");
}

