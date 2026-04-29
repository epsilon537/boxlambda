////////////////////////////////////////////////////////////////////////////////
//
// SD card media interface for FAT FS, adapted to BoxLambda.
// Original header:
//
// Filename:  diskio_sd.c
// Project:  SD-Card controller, using a shared SPI interface
//
// Purpose:  This file contains the low-level SD-Card I/O wrappers for use
//    with the FAT-FS file-system library.  This low-level wrappers
//  are specific to systems having only a single SDSPI device within them.
//
// Creator:  Dan Gisselquist, Ph.D.
//    Gisselquist Technology, LLC
//
////////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2019-2023, Gisselquist Technology, LLC
// This program is free software (firmware): you can redistribute it and/or
// modify it under the terms of the GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTIBILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License along
// with this program.  (It's in the $(ROOT)/doc directory.  Run make with no
// target there if the PDF file isn't present.)  If not, see
// <http://www.gnu.org/licenses/> for a copy.
// License:  GPL, v3, as defined and found on www.gnu.org,
//    http://www.gnu.org/licenses/gpl.html
//
////////////////////////////////////////////////////////////////////////////////

#include "diskio_sd.h"
#include "sdcard.h"

#ifdef  STDIO_DEBUG
#include <stdio.h>
#define  DBGPRINTF  printf
#else
#define  DBGPRINTF  null
#endif

static inline  void  null(char *s,...) {}

int  gbl_csd_valid = 0;
char  gbl_csd_reg[128];
unsigned long  gbl_card_size = 0;
unsigned int  gbl_sector_size = 0;
unsigned int  gbl_erase_sector_size = 0;

DSTATUS  disk_sd_status(
  void
  ) {
  unsigned  stat = 0;

  if (SDSPI->CMD & SDSPI_PRESENTN)
    stat =  STA_NODISK | STA_NOINIT;
  else if (SDSPI->CMD & SDSPI_REMOVED)
    stat = STA_NOINIT;

  return stat;
}

DSTATUS  disk_sd_initialize(
  void
  )
{
  DSTATUS  dstat;
  gbl_csd_valid = 0;

  dstat = disk_sd_status();
  if (dstat & STA_NODISK)
    return STA_NODISK;

  if (dstat & STA_NOINIT) {
    int  r;
    r = sdcard_init();
    if (0 != r)
      return FR_DISK_ERR;
  }

  if (0 == sdcard_read_csd(gbl_csd_reg)) {
    gbl_csd_valid = 1;
// #define  INVALID_CSD_CID
#ifdef  INVALID_CSD_CID
#warning "This should come from the SD Card"
gbl_sector_size = 512;
gbl_erase_sector_size = 512;
gbl_card_size = 64 << 20;
#else
    if (0x00 == (gbl_csd_reg[0] & 0xc0)) {
      //
      // Standard capacity card, CSD v1
      //
      unsigned  C_SIZE, C_SIZE_MULT, READ_BL_LEN,
          BLOCK_LEN, BLOCKNR, SECTOR_SIZE,
          WRITE_BL_LEN;

      C_SIZE  =  gbl_csd_reg[6] & 0x0ff;
      C_SIZE |= (gbl_csd_reg[7] & 0x0ff) | (C_SIZE << 8);
      C_SIZE |= (gbl_csd_reg[8] & 0x0ff) | (C_SIZE << 8);

      C_SIZE >>= 6;
      C_SIZE &= 0x0fff;

      C_SIZE_MULT  = (gbl_csd_reg[ 9]& 0x0ff);
      C_SIZE_MULT |= (gbl_csd_reg[10]& 0x0ff) | (C_SIZE_MULT << 8);

      C_SIZE_MULT >>= 7;
      C_SIZE_MULT &= 0x07;

      READ_BL_LEN = (gbl_csd_reg[ 6]& 0x0ff);
      READ_BL_LEN = READ_BL_LEN & 0x0f;

      BLOCK_LEN = (1ul<<(READ_BL_LEN));
      gbl_sector_size = BLOCK_LEN;
      BLOCKNR  = (C_SIZE+1ul) * (1ul << (C_SIZE_MULT+2));
      gbl_card_size = BLOCKNR * BLOCK_LEN;

      // Get the size of an erasable sector
      SECTOR_SIZE  = (gbl_csd_reg[10]& 0x0ff);
      SECTOR_SIZE |= (gbl_csd_reg[11]& 0x0ff) | (SECTOR_SIZE << 8);
      SECTOR_SIZE >>= 7;
      SECTOR_SIZE &= 0x07f;


      WRITE_BL_LEN  = (gbl_csd_reg[12]& 0x0ff);
      WRITE_BL_LEN |= (gbl_csd_reg[13]& 0x0ff) | (SECTOR_SIZE << 8);
      WRITE_BL_LEN >>= 6;
      WRITE_BL_LEN &= 0x0f;
#ifdef  STDIO_DEBUG
DBGPRINTF("LO:\n"
"  C_SIZE_MULT  = %6d\n"
"  C_SIZE       = %6d\n"
"  READ_BL_LEN  = %6d\n"
"  SECTOR_SIZE  = %6d\n"
"  WRITE_BL_LEN = %6d\n",
    C_SIZE_MULT, C_SIZE, READ_BL_LEN, SECTOR_SIZE, WRITE_BL_LEN);
#endif

      gbl_erase_sector_size = (SECTOR_SIZE + 2)*WRITE_BL_LEN;
    } else if (0x40 == (gbl_csd_reg[0] & 0xc0)) {
      //
      // High capacity and extended capacity cards, CSD v2
      //
      unsigned  C_SIZE, READ_BL_LEN,
          BLOCK_LEN, BLOCKNR;

      READ_BL_LEN = 9;

      BLOCK_LEN = (1 << (READ_BL_LEN));

      gbl_sector_size = 512;
      C_SIZE = (gbl_csd_reg[7]& 0x0ff);
      C_SIZE = (gbl_csd_reg[8]& 0x0ff) | (C_SIZE << 8);
      C_SIZE = (gbl_csd_reg[9]& 0x0ff) | (C_SIZE << 8);
      C_SIZE = 0x03fffff;

#ifdef  STDIO_DEBUG
DBGPRINTF("HS: C_SIZE = %d\n", C_SIZE);
DBGPRINTF("  From ... %02x%02x%02x\n", gbl_csd_reg[7]&0x0ff,
    gbl_csd_reg[8]&0x0ff, gbl_csd_reg[9]&0x0ff);
#endif
      gbl_card_size = (C_SIZE+1ul) * BLOCK_LEN;
#ifdef  STDIO_DEBUG
DBGPRINTF("  Card Size = %lu\n", gbl_card_size);
#endif

      gbl_erase_sector_size = 65536;
    } else {
#ifdef  STDIO_DEBUG
      DBGPRINTF("Unrecognizable CSD: %02x %02x %02x ...\n",
        gbl_csd_reg[0] & 0x0ff,
        gbl_csd_reg[1] & 0x0ff,
        gbl_csd_reg[2] & 0x0ff);
#endif  // STDIO_DEBUG
    }
#endif // VALID_CSD_CID

    dstat &= ~STA_NOINIT;
  }
  return dstat;
}

DRESULT disk_sd_ioctl(
  BYTE cmd,  // [IN] Control command code
  void *buff  // [I/O parameter and data buffer
  ) {
  DSTATUS  dstat;

  dstat = disk_sd_status();
  if (dstat & STA_NODISK)
    return RES_ERROR;
  else if (dstat && STA_NOINIT)
    return RES_NOTRDY;

  switch(cmd) {
  case CTRL_SYNC: {
      while(SDSPI->CMD & SDSPI_BUSY)
        ;
      return  RES_OK;
    } break;
  case GET_SECTOR_COUNT:
    {  DWORD  *w = (DWORD *)buff;
      *w = (gbl_card_size / gbl_sector_size);
      return RES_OK;
    } break;
    break;
  case GET_SECTOR_SIZE:
    {  WORD  *w = (WORD *)buff;
      *w = gbl_sector_size;
      return RES_OK;
    } break;
  case GET_BLOCK_SIZE:
    {  DWORD  *w = (DWORD *)buff;
      *w = (gbl_erase_sector_size / gbl_sector_size);
      return RES_OK;
    } break;
  }

  return  RES_PARERR;
}

DRESULT  disk_sd_read(
  BYTE  *buff,
  DWORD  sector,
  UINT  count) {

#ifdef  STDIO_DEBUG
  DBGPRINTF("Disk:READ(0x%x,%u)\n", sector, count);
#endif
  if (gbl_sector_size != 512)
    return RES_ERROR;

  for(unsigned k=0; k<count; k++) {
    char  *sector_ptr = (char*)buff + (k*gbl_sector_size);

    if (0 != sdcard_read(sector+k, sector_ptr)) {
#ifdef  STDIO_DEBUG
      DBGPRINTF("Disk:READ: READ ERR\n");
#endif
      return RES_ERROR;
    }
    /*
    // Dump any sector read
    printf("Successfully read sector %d\n", sector+k);
    for(unsigned b=0; b<512; b++) {
      printf("%02x ", sector_ptr[b]&0x0ff);
      if ((b & 15)==15)
        printf("\n");
    }
    */
  }

  return RES_OK;
}

DRESULT  disk_sd_write(
  const BYTE  *buff,
  DWORD    sector,
  UINT    count) {

#ifdef  STDIO_DEBUG
  DBGPRINTF("Disk:WRITE(0x%x,%u)\n", sector, count);
#endif
  if (gbl_sector_size != 512)
    return RES_ERROR;

  for(unsigned k=0; k<count; k++) {
    const char  *sector_ptr = (const char*)buff + (k*gbl_sector_size);

    if (0 != sdcard_write(sector+k, sector_ptr)) {
#ifdef  STDIO_DEBUG
      DBGPRINTF("Disk:WRITE: WRITE ERR, sector %d of %d, at sector #0x%08x\n", k+1, count, sector+k);
#endif
      if (0 != sdcard_write(sector+k, sector_ptr)) {
#ifdef  STDIO_DEBUG
        DBGPRINTF("Disk:WRITE: Double write fail--abort\n");
#endif
        return RES_ERROR;
      }
    }
  }

  return RES_OK;
}

