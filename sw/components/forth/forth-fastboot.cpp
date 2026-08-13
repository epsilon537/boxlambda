#include "forth-fastboot.h"
#include "fatal.h"
#include "ff.h"
#include "forth.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

void forth_save_state() {
  const TCHAR *img_path = (const TCHAR *)forth_popda();
  char *emem_end_addr = (char *)forth_popda();
  FIL fil;

  FRESULT res = f_open(&fil, img_path, FA_CREATE_ALWAYS | FA_WRITE);
  if (res == FR_OK) {
    UINT bw;

    res = f_write(&fil, (const void *)&__forth_imem_start,
                  &__forth_imem_end - &__forth_imem_start, &bw);
    if (res == FR_OK) {
      res = f_write(&fil, (const void *)&__forth_emem_start,
                    emem_end_addr - &__forth_emem_start, &bw);

      if (res == FR_OK) {
        res = f_close(&fil);
      }
    }
  }

  forth_pushda(res);
}

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
  // ( emem_end_addr cstr --- )
  forth_register_cfun(forth_save_state, "forth-save-state");
}
