#include <assert.h>
#include <stdio.h>
#include "embedded_cli.h"
#include "ff.h"
#include <stdlib.h>
#include "mem_fs_cli.h"

#define STR_ROOT_DIRECTORY ""

/* mem_fs_cli is an embedded_cli client component providing convenience commands */
/* for file system access and memory management. */

extern "C" {
  /* Helper function listing contents of a directory */
  static FRESULT list_dir(const char *path) {
    FRESULT res;
    DIR dir;
    FILINFO fno;
    int nfile, ndir;

    res = f_opendir(&dir, path); /* Open the directory */
    if (res == FR_OK) {
        nfile = ndir = 0;
        for (;;) {
            res = f_readdir(&dir, &fno);                   /* Read a directory item */
            if (res != FR_OK || fno.fname[0] == 0) break;  /* Error or end of dir */
            if (fno.fattrib & AM_DIR) {            /* Directory */
                printf("   <DIR>   %s\n", fno.fname);
                ndir++;
            } else {                               /* File */
                printf("%10u %s\n", fno.fsize, fno.fname);
                nfile++;
            }
        }
        f_closedir(&dir);
        printf("%d dirs, %d files.\n", ndir, nfile);
    } else {
        printf("Failed to open \"%s\". (%u)\n", path, res);
    }
    return res;
  }

  //CLI command to remove a file.
  static void rm(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 1) {
        printf("Argument missing: rm <filename>\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);

      FRESULT res = f_unlink(filename);
      if (res != FR_OK) {
        printf("FatFS file unlinke error! Erro code:%d.\n", res);
        return;
      }

      printf("Removed file %s.\n", filename);
    }
  }

  //CLI command to save to contents of a given memory range to a file.
  static void save(EmbeddedCli *cli, char *args, void *context) {
    if (embeddedCliGetTokenCount(args) < 3) {
        printf("Argument missing: save <filename> <address> <size in bytes>\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);
      const char *addrString = embeddedCliGetToken(args, 2);
      const char *sizeString = embeddedCliGetToken(args, 3);
      uint32_t addr, size;

      sscanf(addrString, "%08X", &addr);
      sscanf(sizeString, "%d", &size);

      printf("Saving memory range 0x%x-0x%x (%d bytes) to file %s.\n", addr, addr+size, size, filename);

	    FIL file_object;

      /* Create a new file */
      FRESULT res = f_open(&file_object, (char const *)filename, FA_CREATE_ALWAYS | FA_WRITE);
      if (res != FR_OK) {
        printf("FatFS file open error!\n");
        return;
      }

      UINT byte_written;
      res = f_write(&file_object, (const void*)addr, size, &byte_written);

      if (res != FR_OK) {
        printf("FatFS file write error!\n");
         return;
      }

      f_close(&file_object);
    }
  }

  //CLI command to load the contents of a given file into memory.
  static void load(EmbeddedCli *cli, char *args, void *context) {

    uint16_t tokenCount = embeddedCliGetTokenCount(args);
    if (tokenCount < 1) {
        printf("Argument missing: load <filename> [address]\n");
    }
    else {
      const char *filename = embeddedCliGetToken(args, 1);
	    FIL file_object;
      uint32_t addr;
      uint32_t size;

      /* Open the file */
      FRESULT res = f_open(&file_object, (char const *)filename, FA_OPEN_EXISTING | FA_READ);
      if (res != FR_OK) {
        printf("FatFS file open error! Error code: %d\n", res);
        return;
      }

      size = f_size(&file_object);

      if (tokenCount == 2) {
        const char *addrString = embeddedCliGetToken(args, 2);
        sscanf(addrString, "%08X", &addr);
      }
      else {
        addr = (uint32_t)malloc(size);
        assert(addr);
      }

      printf("Loading file %s, size: %d bytes, into memory at address 0x%x.\n", filename, size, addr);

      /* Read file */
      UINT bytes_read;
      res = f_read(&file_object, (void*)addr, size, &bytes_read);
      if (res != FR_OK) {
        printf("FatFS file read error! Error code: %d\n", res);
        return;
      }

      /* Close the file*/
      f_close(&file_object);
    }
  }

  //CLI command to list the current directory's contents.
  static void ls(EmbeddedCli *cli, char *args, void *context) {
    FRESULT res =list_dir(STR_ROOT_DIRECTORY);
    if (res != FR_OK) {
      printf("FatFS list_dir() error. Error code: %d\n", res);
      return;
    }
  }

  //CLI command to allocate a block of memory using the C run-times memory
  //allocator.
  static void allocBuf(EmbeddedCli *cli, char *args, void *context) {

    if (embeddedCliGetTokenCount(args) < 1) {
      printf("Argument missing: allocBuf <size in bytes>\n");
    }
    else {
      const char *sizeString = embeddedCliGetToken(args, 1);
      uint32_t addr, size;

      sscanf(sizeString, "%d", &size);
      addr = (uint32_t)malloc(size);

      printf("Allocate buffer at address 0x%x, size: %d bytes.\n", addr, size);
    }
  }

  //CLI command to release/free an memory block previously allocated using the
  //allocBuf command above.
  static void relBuf(EmbeddedCli *cli, char *args, void *context) {

    if (embeddedCliGetTokenCount(args) < 1) {
        printf("Argument missing: reBuf <address>\n");
    }
    else {
      const char *addrString = embeddedCliGetToken(args, 2);
      uint32_t addr;

      sscanf(addrString, "%08X", &addr);

      printf("Releasing buffer at memory address 0x%x\n", addr);

      free((void*)addr);
    }
  }
}

//Call this function to hook the above CLI commands into the embedded CLI instance
//running on the system.
void add_mem_fs_cli(EmbeddedCli* cli) {
  assert(cli);

  embeddedCliAddBinding(cli, {
        "rm",          // command name (spaces are not allowed)
        "rm <filename> : Remove (delete) file.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        rm               // binding function
  });

  embeddedCliAddBinding(cli, {
        "save",          // command name (spaces are not allowed)
        "save <filename> <address> <size in bytes> : write memory contents to file.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        save               // binding function
  });

  embeddedCliAddBinding(cli, {
        "load",          // command name (spaces are not allowed)
        "load <filename> [address] : read file into memory. Alloc mem. buf. if addr. not given.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        load               // binding function
  });

  embeddedCliAddBinding(cli, {
        "allocBuf",          // command name (spaces are not allowed)
        "allocBuf <size> : allocate from heap buffer of given size.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        allocBuf
  });

  embeddedCliAddBinding(cli, {
        "relBuf",          // command name (spaces are not allowed)
        "relBuf <address> : release the buffer at given adress.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        relBuf
  });

  embeddedCliAddBinding(cli, {
        "ls",          // command name (spaces are not allowed)
        "list directory contents.",   // Optional help for a command (NULL for no help)
        true,              // flag whether to tokenize arguments (see below)
        nullptr,            // optional pointer to any application context
        ls               // binding function
  });
}

