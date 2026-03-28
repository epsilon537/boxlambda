// BoxLambda FATFS Media interface dispatching layer

#include "ff.h"    // From FATFS
#include "diskio.h"  // From FATFS as well
#include "diskio_sd.h" // SD Media Interface
#include "diskio_ram.h" // RAM Media Interface

typedef struct {
  DSTATUS (*status)(void);
  DSTATUS (*initialize)(void);
  DRESULT (*read)(BYTE*, DWORD, UINT);
  DRESULT (*write)(const BYTE*, DWORD, UINT);
  DRESULT (*ioctl)(BYTE, void*);
} disk_driver_t;

static const disk_driver_t drivers[] = {
  { disk_sd_status, disk_sd_initialize, disk_sd_read, disk_sd_write, disk_sd_ioctl },
  { disk_ram_status, disk_ram_initialize_fatfs, disk_ram_read, disk_ram_write, disk_ram_ioctl },
};

// pdrv==0 -> SD
// pdfrv==1 -> RAM

DSTATUS  disk_status( BYTE pdrv)
{
  if (pdrv >= sizeof(drivers)/sizeof(drivers[0]))
    return STA_NOINIT;

  return drivers[pdrv].status();
}

DSTATUS disk_initialize(BYTE pdrv)
{
    if (pdrv >= sizeof(drivers)/sizeof(drivers[0]))
      return STA_NOINIT;

    return drivers[pdrv].initialize();
}

DRESULT disk_read(BYTE pdrv, BYTE *buff, DWORD sector, UINT count) {
    if (pdrv >= sizeof(drivers)/sizeof(drivers[0]))
      return RES_ERROR;

    return drivers[pdrv].read(buff, sector, count);
}

DRESULT disk_write( BYTE pdrv, const BYTE *buff, DWORD  sector, UINT count) {
    if (pdrv >= sizeof(drivers)/sizeof(drivers[0]))
      return RES_ERROR;

    return drivers[pdrv].write(buff, sector, count);
}

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void *buff) {
    if (pdrv >= sizeof(drivers)/sizeof(drivers[0]))
       return RES_ERROR;

    return drivers[pdrv].ioctl(cmd, buff);
}

// Shared by both media interfaces.
DWORD  get_fattime(void) {
  DWORD  result;
  unsigned  thedate, clocktime;

  thedate = 0x20191001;
  clocktime = 0x0; // Midnight

#ifdef  _BOARD_HAS_RTC
  clocktime = _rtc->r_clock;
#endif

  unsigned year, month, day, hrs, mns, sec;

  year =  ((thedate & 0xf0000000)>>28)*1000 +
    ((thedate & 0x0f000000)>>24)*100 +
    ((thedate & 0x00f00000)>>20)*10 +
    ((thedate & 0x000f0000)>>16);
  year -= 1980;

  month = ((thedate & 0x00f000)>>12)*10 +
    ((thedate & 0x000f00)>> 8);

  day   = ((thedate & 0x00f0)>> 4)*10 +
    ((thedate & 0x000f)    );

  hrs   = ((clocktime & 0xf00000)>>20)*10 +
    ((clocktime & 0x0f0000)>>16);

  mns   = ((clocktime & 0xf000)>>12)*10 +
    ((clocktime & 0x0f00)>> 8);

  sec   = ((clocktime & 0xf0)>> 4)*10 +
    ((clocktime & 0x0f));

  result = (sec & 0x01f) | ((mns & 0x3f)<<5)
    | ((hrs & 0x01f)<<11)
    | ((day & 0x01f)<<16)
    | ((month & 0x0f)<<21)
    | ((year & 0x7f)<<25);

  return result;
}

