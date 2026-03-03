#ifndef DISKIO_SD_H
#define DISKIO_SD_H

#include "ff.h"    // From FATFS
#include "diskio.h"  // From FATFS as well

DSTATUS  disk_sd_status(
  void
  );

DSTATUS  disk_sd_initialize(
  void
  );

DRESULT disk_sd_ioctl(
  BYTE cmd,  // [IN] Control command code
  void *buff  // [I/O parameter and data buffer
  );

DRESULT  disk_sd_read(
  BYTE  *buff,
  DWORD  sector,
  UINT  count);

DRESULT  disk_sd_write(
  const BYTE  *buff,
  DWORD    sector,
  UINT    count);

#endif /*DISKIO_SD_H*/
