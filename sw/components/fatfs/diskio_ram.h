#ifndef DISKIO_RAM_H
#define DISKIO_RAM_H

#include "ff.h"    // From FATFS
#include "diskio.h"  // From FATFS as well

// Call this before initializing FAT FS
// The RAM disk module requires a separate init function because
// because the FATFS media interface init function takes no parameters.
void disk_ram_init(unsigned char *fs_image_start, unsigned long fs_image_size);

//
// The RAM disk FAT FS Media Access Interface
//

DSTATUS  disk_ram_status(
  void
  );

// This is used by FATFS. Should not be called from application code.
DSTATUS  disk_ram_initialize_fatfs(
  void
  );

DRESULT disk_ram_ioctl(
  BYTE cmd,  // [IN] Control command code
  void *buff  // [I/O] parameter and data buffer
  );

DRESULT  disk_ram_read(
  BYTE  *buff,
  DWORD  sector,
  UINT  count);

DRESULT  disk_ram_write(
  const BYTE  *buff,
  DWORD    sector,
  UINT    count);

#endif /*DISKIO_RAM_H*/
