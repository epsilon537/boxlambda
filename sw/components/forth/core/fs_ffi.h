#ifndef FS_FFI_H
#define FS_FFI_H

#include "ff.h"

typedef struct {
  FATFS vol;
  const char *name;
} Fs_Volume_t;

void fs_ffi_init(Fs_Volume_t vols[], unsigned num_vols);

#endif //FS_FFI_H

