#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include <stdlib.h>
#include "uart.h"
#include "mcycle.h"
#include "ff.h"
#include "diskio_ram.h"

#define STR_ROOT_DIRECTORY ""
const char *file_name = STR_ROOT_DIRECTORY "Basic.bin";
const char *text_file_name = STR_ROOT_DIRECTORY "log.txt";

/** Size of the file to write/read.*/
#define DATA_SIZE 2048
uint8_t data_buffer[DATA_SIZE];
#define TEST_SIZE   (4 * 1024)

uint8_t mkfs_work[FF_MAX_SS];

#define RAM_FS_IMAGE_SIZE (128*1024)
uint8_t ram_fs_image[RAM_FS_IMAGE_SIZE];

#ifdef __cplusplus
extern "C" {
#endif

//_init is executed by picolibc startup code before main().
void _init(void) {
}

//_exit is executed by the picolibc exit function.
//An implementation has to be provided to be able to user assert().
void  _exit (int status) {
  while (1);
}

static FATFS sdfs;
static FATFS ramfs;
static FIL file_object;

#ifdef __cplusplus
}
#endif

//This FatFs test function is derived from avrxml/asf's run_fatfs_test.
//
//Returns 0 if OK. Negative on error
static int fatfs_test(FATFS* fatfs, const char* volname, bool mkfs)
{
  uint32_t i;
  UINT byte_to_read;
  UINT byte_read;
  UINT byte_written;

  FRESULT res;
  DIR dirs;

  if (mkfs) {
    printf("Formatting...\n");
    res = f_mkfs (
      volname, /*path*/
      0, /*opt*/
      mkfs_work, /*work*/
      FF_MAX_SS); /*len*/

    if (res != FR_OK) {
      printf("FatFS mkfs error! %d\n", res);
        return -1;
    }
  }

  printf("Mounting...\n");
  /* Clear file system object */
  memset(fatfs, 0, sizeof(FATFS));
  res = f_mount(fatfs, volname, 1);
  if (res != FR_OK) {
    printf("FatFS mount error! %d\n", res);
      return -1;
  }

  res = f_chdrive(volname);
  if (res != FR_OK) {
    printf("FatFS chdrive error! %d\n", res);
      return -1;
  }

  printf("Opendir...\n");
  /* Test if the disk is formated */
  res = f_opendir(&dirs, STR_ROOT_DIRECTORY);
  if (res != FR_OK) {
    printf("FatFS opendir error!\n");
      return -1;
  }

  printf("Creating file...\n");
  /* Create a new file */
  res = f_open(&file_object, (char const *)file_name,
      FA_CREATE_ALWAYS | FA_WRITE);
  if (res != FR_OK) {
    printf("FatFS file open error!\n");
      return -1;
  }

  /* Write a checkerboard pattern in the buffer */
  for (i = 0; i < sizeof(data_buffer); i++) {
    if ((i & 1) == 0) {
      data_buffer[i] = (i & 0x55);
    } else {
      data_buffer[i] = (i & 0xAA);
    }
  }

  printf("Writing...\n");
  for (i = 0; i < TEST_SIZE; i += DATA_SIZE) {
    res = f_write(&file_object, data_buffer, DATA_SIZE,
        &byte_written);

    if (res != FR_OK) {
      printf("FatFS file write error!\n");
          return -1;
    }
  }

  /* Close the file */
  printf("Closing file...\n");
  res = f_close(&file_object);
  if (res != FR_OK) {
    printf("FatFS file close error!\n");
      return -1;
  }

  /* Open the file */
  printf("Re-opening file for reading...\n");
  res = f_open(&file_object, (char const *)file_name,
      FA_OPEN_EXISTING | FA_READ);
  if (res != FR_OK) {
    printf("FatFS file open error!\n");
      return -1;
  }

  /* Read file */
  printf("Reading...\n");
  memset(data_buffer, 0, DATA_SIZE);
  byte_to_read = f_size(&file_object);

  for (i = 0; i < byte_to_read; i += DATA_SIZE) {
    res = f_read(&file_object, data_buffer, DATA_SIZE, &byte_read);
    if (res != FR_OK) {
      printf("FatFS file read error!\n");
          return -1;
    }
  }

  /* Close the file*/
  printf("Closing file...\n");
  res = f_close(&file_object);
  if (res != FR_OK) {
    printf("FatFS file close error!\n");
      return -1;
  }

  /* Compare read data with the expected data */
  printf("Comparing...\n");
  for (i = 0; i < sizeof(data_buffer); i++) {
    if (!((((i & 1) == 0) && (data_buffer[i] == (i & 0x55))) ||
        (data_buffer[i] == (i & 0xAA)))) {
      printf("FatFS data compare error!\n");
          return -1;
    }
  }

  /* Create a new file */
  printf("f_printf test...\n");
  res = f_open(&file_object, (char const *)text_file_name,
      FA_CREATE_ALWAYS | FA_WRITE);
  if (res != FR_OK) {
    printf("FatFS file open error!\n");
      return -1;
  }

  if (f_printf(&file_object, "This is a test.\n") < 0) {
    printf("FatFS f_printf error!\n");
    return -1;
  }

  /* Close the file*/
  res = f_close(&file_object);
  if (res != FR_OK) {
    printf("FatFS file close error!\n");
      return -1;
  }

  return 0;
}

int main(void) {
  uint32_t leds = 0xF;

  printf("Starting fatfs_test on SD...\n");

  /*SD FS test*/
  if (fatfs_test(&sdfs, "0:", /*mkfs*/ false) != 0) {
    printf("SD Filesystem test failed.\n");
    return -1;
  }

  printf("Starting fatfs_test on RAM disk...\n");
  /*RAM FS test*/
  disk_ram_init((unsigned char *)ram_fs_image, (unsigned long)(RAM_FS_IMAGE_SIZE));
  if (fatfs_test(&ramfs, "1:", /*mkfs*/ true) !=0) {
    printf("RAM Filesystem test failed.\n");
    return -1;
  }

  printf("Test Successful.\n");

  return 0;
}

