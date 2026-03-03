#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

#include "gpio.h"
#include "forth.h"
#include "fs_ffi.h"
#include "stdio_redirect_ffi.h"
#include "diskio_ram.h"
#include "ff.h"
#include "init.fs"
#include "heap.fs"
#include "cstr.fs"
#include "escstr.fs"
#include "pool.fs"
#include "temp-alloc.fs"
#include "istr.fs"
#include "printf.fs"
#include "fs.fs"
#include "fs_redirect.fs"
#include "shell.fs"
#include "conditional-comp.fs"

#define GPIO_SIM_INDICATOR 0xf0 //If GPIO inputs 7:4 have this value, this is a simulation.

#ifdef __cplusplus
extern "C" {
#endif
//_init is executed by picolibc startup code before main().
void _init(void) {
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void	_exit (int status) {
	while (1);
}

//The RAM FS image location.
extern char __fs_image_start[];
extern char __fs_image_size[];

// The file system objects
FATFS fs_sd;
FATFS fs_ram;

#ifdef __cplusplus
}
#endif

int main(void) {
  uint32_t leds = 0xF;

  gpio_init();
  gpio_set_direction(0x0000000F); //4 outputs, 20 inputs

  printf("Initializing Forth...\n");

  forth_core_init();
  stdio_redirect_ffi_init();

  printf("Forth core init complete.\n");

  printf("Compiling init.fs...\n");

  forth_load_buf((char*)init_fs, /*verbose=*/ false);

  printf("Compiling heap.fs...\n");

  forth_load_buf((char*)heap_fs, /*verbose=*/ false);

  printf("Compiling pool.fs...\n");

  forth_load_buf((char*)pool_fs, /*verbose=*/ false);

  printf("Compiling temp-alloc.fs...\n");

  forth_load_buf((char*)temp_alloc_fs, /*verbose=*/ false);

  printf("Compiling istr.fs...\n");

  forth_load_buf((char*)istr_fs, /*verbose=*/ false);

  printf("Compiling escstr.fs...\n");

  forth_load_buf((char*)escstr_fs, /*verbose=*/ false);

  printf("Compiling printf.fs...\n");

  forth_load_buf((char*)printf_fs, /*verbose=*/ false);

  printf("Mounting file system...\n");
  /* Clear file system object */
  memset(&fs_sd, 0, sizeof(FATFS));

  FRESULT res = f_mount(&fs_sd, "0:", 1);
  if (res != FR_OK)
  {
    printf("SD FatFS mount error! %d\n", res);
    return -1;
  }

  disk_ram_init((unsigned char *)__fs_image_start, (unsigned long)__fs_image_size);

  memset(&fs_ram, 0, sizeof(FATFS));

  res = f_mount(&fs_ram, "1:", 1);
  if (res != FR_OK)
  {
    printf("RAM FatFS mount error! %d\n", res);
    return -1;
  }

  printf("Initializing Forth Filesystem FFI...\n");

  fs_ffi_init();

  printf("Forth FS FFI init complete.\n");

  printf("Loading cstr.fs...\n");

  forth_load_buf((char*)cstr_fs, /*verbose=*/ false);

  printf("Loading fs.fs...\n");

  forth_load_buf((char*)fs_fs, /*verbose=*/ false);

  printf("Loading fs_redirect.fs...\n");

  forth_load_buf((char*)fs_redirect_fs, /*verbose=*/ false);

  printf("Loading shell.fs...\n");

  forth_load_buf((char*)shell_fs, /*verbose=*/ false);

  printf("Loading conditional-comp.fs...\n");

  forth_load_buf((char*)conditional_comp, /*verbose=*/ false);

  forth_execute_word("welcome");

  forth_eval(".( Ready. ) cr");

  forth_repl();

  printf("\nForth REPL exited.\n");

  while (1);
}

