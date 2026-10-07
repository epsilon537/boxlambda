#include "forth-fastboot.h"
#include "fatal.h"
#include "ff.h"
#include "forth.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

void forth_save_state() {
  // Stack signature: ( emem_end_addr cstr --- )

  const TCHAR *img_path = (const TCHAR *)forth_popda();
  char *emem_end_addr = (char *)forth_popda();
  FIL fil;

  FRESULT res = f_open(&fil, img_path, FA_CREATE_ALWAYS | FA_WRITE);
  if (res == FR_OK) {
    UINT btw, bw;

    btw = &__forth_imem_end - &__forth_imem_start;
    // Save all of forth_imem
    res = f_write(&fil, (const void *)&__forth_imem_start, btw, &bw);
    if (bw < btw)
      die("Can't save %s, disk full.\n", img_path);

    if (res == FR_OK) {
      btw = emem_end_addr - &__forth_emem_start;
      // Save forth_emem up to the given emem end address.
      res = f_write(&fil, (const void *)&__forth_emem_start, btw, &bw);
      if (bw < btw)
        die("Can't save %s, disk full.\n", img_path);

      if (res == FR_OK) {
        res = f_close(&fil);
      }
    }
  }

  forth_pushda(res);
}

// The counterpart of the save-state function above, restoring the Forth
// state using given image.
void forth_fastboot_load(const char *boxkern_forth_image_start,
                         const char *boxkern_forth_image_end) {
  uint32_t forth_imem_size = &__forth_imem_end - &__forth_imem_start;

  assert(boxkern_forth_image_end - boxkern_forth_image_start > forth_imem_size);

  memcpy(&__forth_imem_start, boxkern_forth_image_start, forth_imem_size);
  uint32_t emem_img_size =
      boxkern_forth_image_end - boxkern_forth_image_start - forth_imem_size;
  memcpy(&__forth_emem_start, boxkern_forth_image_start + forth_imem_size,
         emem_img_size);
}

void forth_fastboot_init() {
  forth_register_cfun(forth_save_state, "forth-save-state");
}
