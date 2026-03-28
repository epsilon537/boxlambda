// RAM disk media interface for FATFS.
//
#include "diskio_ram.h"
#include <string.h>

#define RAM_DISK_SECTOR_SIZE 512

// Pointer to and size of the memory area where the RAM disk will live.
unsigned char *ram_disk_ptr = 0;
unsigned long ram_disk_size = 0;

// The RAM disk module requires a separate init function because
// because the FATFS media interface init function takes no parameters.
void disk_ram_init(unsigned char *fs_image_start, unsigned long fs_image_size) {
  ram_disk_ptr = fs_image_start;
  ram_disk_size = fs_image_size;
}

DSTATUS disk_ram_status(void) {
  unsigned stat = 0;

  if ((ram_disk_ptr == 0) || (ram_disk_size == 0))
    stat = STA_NOINIT;

  return stat;
}

DSTATUS disk_ram_initialize_fatfs(void) {
  return disk_ram_status();
}

DRESULT disk_ram_ioctl(BYTE cmd, void *buff) {
  DSTATUS  dstat;

  dstat = disk_ram_status();
  if (dstat & STA_NODISK)
    return RES_ERROR;
  else if (dstat && STA_NOINIT)
    return RES_NOTRDY;

  switch(cmd) {
    case CTRL_SYNC:
      return RES_OK;
    case GET_SECTOR_COUNT:
      *(WORD *)buff = (ram_disk_size / RAM_DISK_SECTOR_SIZE);
      return RES_OK;
    case GET_SECTOR_SIZE:
      *(WORD *)buff = RAM_DISK_SECTOR_SIZE;
      return RES_OK;
    case GET_BLOCK_SIZE:
      *(WORD *)buff = 1;
      return RES_OK;
  }

  return RES_PARERR;
}

DRESULT  disk_ram_read(
  BYTE  *buff,
  DWORD  sector,
  UINT  count) {

  //Copy count sectors starting from sector to buff.
  memcpy(buff, ram_disk_ptr + sector*RAM_DISK_SECTOR_SIZE, count*RAM_DISK_SECTOR_SIZE);

  return RES_OK;
}

DRESULT  disk_ram_write(
  const BYTE  *buff,
  DWORD    sector,
  UINT    count) {

  //Copy counter sectors from buff to sector
  memcpy(ram_disk_ptr + sector*RAM_DISK_SECTOR_SIZE, buff, count*RAM_DISK_SECTOR_SIZE);

  return RES_OK;
}

