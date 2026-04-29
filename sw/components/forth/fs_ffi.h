#ifndef FS_FFI_H
#define FS_FFI_H

// FATFS API
#include "ff.h"
#include "inout.h"

typedef struct {
  FATFS vol;
  const char *name;
} Fs_Volume_t;

// Filesystem Forth FFI (Foreign Function Interface) Initialization.
// This function registers the necessary FATFS functions and data structure elements with Forth.
// @param: vols[]: array of structs contains FATFS volume object along with the volume's name (e.g. "ram:", "sd:")
// @param: num_vols: Number of elements in the vols[] array.
void fs_ffi_init(IN Fs_Volume_t vols[], unsigned num_vols);

#endif //FS_FFI_H

