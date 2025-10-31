/**
 *  BoxLambda port of Mecrisp Cube's fs.c module by
 *  Ruben Lysens / Epsilon537.
 *
 *  Original file header:
 *
 *  @brief
 *      FAT filesystem for Secure Digital Memory Card and Internal Flash.
 *
 *  Some file tools like GNU tools e.g. ls, pwd, cat.
 *  Do not expect real UNIX commands not even comparable to the Busybox
 *  commands. The UNIX like shell commands are parsing words. The parameters
 *  are parsed from the input stream till the end of line.
 *  These commands are not intended to use in other words, they are used
 *  in the interpreter mode.
 *  @file
 *      fs.c
 *  @author
 *      Peter Schmid, peter@spyr.ch
 *  @date
 *      2020-06-03
 *  @remark
 *      Language: C, STM32CubeIDE GCC
 *  @copyright
 *      Peter Schmid, Switzerland
 *
 *      This project Mecrsip-Cube is free software: you can redistribute it
 *      and/or modify it under the terms of the GNU General Public License
 *      as published by the Free Software Foundation, either version 3 of
 *      the License, or (at your option) any later version.
 *
 *      Mecrsip-Cube is distributed in the hope that it will be useful, but
 *      WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *      General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with Mecrsip-Cube. If not, see http://www.gnu.org/licenses/.
 */

// System include files
// ********************
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
// #include <ctype.h>
// #include <fd.h>


// Application include files
// *************************
#include "fs.h"
// #include "sd.h"
#include "ff.h" // Fat FS
#include "rtc.h"
#include "forth.h"

// Defines
// *******
#define LINE_LENGTH    256
#define SCRATCH_SIZE   0x1000     // 4 KiB scratch

// Global Variables
// ****************

const char FS_Version[] = "  * FatFs for microSD - Generic FAT fs module  R0.12c (C) 2017 ChaN\n";

const char FS_Forth[] =  R"FS_Forth(
\ ( any "filename" -- any ) Interprets the content of the file.
: include token included ;
)FS_Forth";

// FATFS FatFs_FD; /* Work area (filesystem object) for logical flash drive (0) */
FATFS FatFs_SD; /* Work area (filesystem object) for logical SD drive (1) */
FILINFO fno; /* File information */
DIR dj;   /* Directory object */

char path[255];
char pattern[20];
char line[300]; /* Line buffer */

// Forth word XTs
uint32_t cr_xt;
uint32_t token_xt;
uint32_t accept_xt;
uint32_t type_xt;

// Private Variables
// *****************
uint8_t mkfs_scratch[SCRATCH_SIZE];

// Public Functions
// ****************

/**
 *  @brief
 *      Initializes the filesystem
 *  @return
 *      None
 */
void FS_init(void) {
 /* Gives a work area to the SD drive */
 f_mount(&FatFs_SD, "", 1);

 cr_xt = forth_find_word("cr");
 token_xt = forth_find_word("token");
 accept_xt = forth_find_word("accept");
 type_xt = forth_find_word("type");

 forth_register_cfun(FS_included, "included");
 forth_register_cfun(FS_cat, "cat");
 forth_register_cfun(FS_ls, "ls");
 forth_register_cfun(FS_cd, "cd");
 forth_register_cfun(FS_pwd, "pwd");
 forth_register_cfun(FS_mkdir, "mkdir");
 forth_register_cfun(FS_rm, "rm");
 forth_register_cfun(FS_chmod, "chmod");
 forth_register_cfun(FS_touch, "touch");
 forth_register_cfun(FS_mv, "mv");
 forth_register_cfun(FS_cp, "cp");
 forth_register_cfun(FS_df, "df");
 forth_register_cfun(FS_date, "date");
 forth_register_cfun(FS_mount, "mount");
 forth_register_cfun(FS_umount, "umount");
 forth_register_cfun(FS_mkfs, "mkfs");

 forth_load_buf((char*)FS_Forth, /*verbose=*/false);
}


/**
 *  @brief
 *      Interprets the content of the file.
 */
void FS_included() {
 FIL fil;        /* File object */
 FRESULT fr;     /* FatFs return code */
 char *line;
 char *path;

 int count = (int)forth_popda();
 uint8_t *str = (uint8_t*)forth_popda();

 line = (char *) malloc(LINE_LENGTH);
 path = (char *) malloc(LINE_LENGTH);

 memcpy(path, str, count);
 path[count] = 0;

 /* Open a text file */
 fr = f_open(&fil, path, FA_READ);
 if (fr) {
  // open failed
  FS_type((uint8_t*)path, strlen(path));
  strcpy(line, ": file not found\n");
  FS_type((uint8_t*)line, strlen(line));
 }

 /* Read every line and interprets it */
 while (f_gets(line, LINE_LENGTH-1, &fil)) {
  // line without \n
  FS_evaluate((uint8_t*)line, strlen(line)-1);
 }

 /* Close the file */
 f_close(&fil);

 free(line);
 free(path);
}


/**
 *  @brief
 *      Concatenate files and print on the standard output
 *
 *      The parameters are taken from the command line (Forth tokens)
 */
void FS_cat() {
 uint8_t *str = NULL;
 int count = 1;
 uint8_t n_flag = false;
 uint8_t outfile_flag = false;
 uint8_t input_flag = false;
 uint8_t EOF_flag = false;
 int line_num = 0;
 FIL fil_in;  /* File object */
 FIL fil_out; /* File object */
 FRESULT fr;  /* FatFs return code */
 BYTE mode;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;
  if (! strcmp(line, "-n")) {
   n_flag = true;
  } else if ( (! strcmp(line, ">")) || (! strcmp(line, ">>")) ) {
   if (! strcmp(line, ">")) {
    // new file
    mode = FA_CREATE_ALWAYS | FA_WRITE;
   } else {
    // append
    mode = FA_OPEN_APPEND | FA_WRITE;
   }
   FS_token(&str, &count);
   memcpy(line, str, count);
   line[count] = 0;
   if (count == 0) {
    // no more tokens
    break;
   }
   outfile_flag = true;
   fr = f_open(&fil_out, line, mode);
   if (fr != FR_OK) {
    FS_type((uint8_t*)line, strlen(line));
    strcpy(line, ": can't create file");
    FS_type((uint8_t*)line, strlen(line));
    break;
   }
  } else if (! strcmp(line, "<<") ) {
   FS_token(&str, &count);
   memcpy(pattern, str, count);
   line[count] = 0;
   if (count == 0) {
    // no more tokens
    break;
   }
   input_flag = true;
  } else {
   /* Open a text file */
   fr = f_open(&fil_in, line, FA_READ);
   if (fr != FR_OK) {
    // open failed
    FS_type((uint8_t*)line, strlen(line));
    strcpy(line, ": file not found");
    FS_type((uint8_t*)line, strlen(line));
   } else {
    /* Read every line and type it */
    while (f_gets(line, sizeof(line), &fil_in)) {
     if (n_flag) {
      snprintf(pattern, sizeof(pattern), "%6i: ", line_num++);
      if (outfile_flag) {
       f_puts(pattern, &fil_out);
      } else {
       FS_type((uint8_t*)pattern, strlen(pattern));
      }
     }
     if (outfile_flag) {
      f_puts(line, &fil_out);
     } else {
      FS_type((uint8_t*)line, strlen(line));
     }
    }
    /* Close the file */
    f_close(&fil_in);
   }
  }
 }

 if (input_flag) {
  // input from console
  while (! EOF_flag) {
   // read till end of line
   count = 255;
   FS_accept((uint8_t*)line, &count);
   line[count] = 0;
   if (! strcmp(line, pattern)) {
    // EOF
    EOF_flag = true;
   } else {
    if (outfile_flag) {
     f_puts(line, &fil_out);
     f_putc('\n', &fil_out);
    } else {
     FS_type((uint8_t*)line, strlen(line));
    }
   }
   FS_cr();
  }
 }

 if (outfile_flag) {
  f_close(&fil_out);
 }
}


/**
 *  @brief
 *      List directory contents.
 */
void FS_ls() {
 char attrib[6];
 uint8_t *str = NULL;
 int count = 1;
 uint8_t a_flag = false;
 uint8_t l_flag = false;
 uint8_t one_flag = false;
 uint8_t param = false;
 uint8_t column = 0;
 FRESULT fr;     /* FatFs return code */

 memset(&dj, 0, sizeof(dj));
 memset(&fno, 0, sizeof(fno));

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   if (!param) {
    line[0] = 0;
   }
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;
  if (! strcmp (line, "-a")) {
   a_flag = true;
  } else if (! strcmp (line, "-l")) {
   l_flag = true;
  } else if (! strcmp (line, "-1")) {
   one_flag = true;
  } else {
   param = true;
  }
 }

 if (strchr(line, '*') != NULL || strchr(line, '?') != NULL) {
  // there is a matching pattern string
  if (strrchr(line, '/') == NULL) {
   // no path, only pattern
   strncpy(pattern, line, sizeof(pattern));
   strcpy(path, "");
  } else {
   // path and pattern
   strncpy(pattern, strrchr(line, '/')+1, sizeof(pattern));
   strncpy(path, line, sizeof(path));
   path[strlen(line) - strlen(pattern) - 1] = 0;
  }
 } else {
  // only pathpattern
  strcpy(pattern, "*");
  strncpy(path, line, sizeof(path));
 }

 fr = f_findfirst(&dj, &fno, path, pattern);

 FS_cr();
 while (fr == FR_OK && fno.fname[0]) {
  /* Repeat while an item is found */
  if (l_flag) {
   strcpy(attrib, "----"); // drwa
   if ( (fno.fattrib & AM_DIR) == AM_DIR) {
    attrib[0] = 'd';
   }
   if ( (fno.fattrib & AM_SYS) != AM_SYS) {
    attrib[1] = 'r';
   }
   if ( (fno.fattrib & AM_RDO) != AM_RDO &&
     (fno.fattrib & AM_SYS) != AM_SYS ) {
    attrib[2] = 'w';
   }
   if ( (fno.fattrib & AM_ARC) == AM_ARC) {
    attrib[3] = 'a';
   }

   snprintf(line, sizeof(line), "%s %9u %4u-%02u-%02uT%02u:%02u:%02u %s\n",
     attrib,
     (unsigned int)fno.fsize,
     (fno.fdate >> 9) + 1980,  (fno.fdate >> 5) & 0xF,  fno.fdate & 0x1F,
     (fno.ftime >> 11) & 0x1F, (fno.ftime >> 5) & 0x2F, (fno.ftime & 0x1F)*2,
     fno.fname);
  } else {
   // not long format
   if (one_flag) {
    // one column
    snprintf(line, sizeof(line), "%s\n", fno.fname);
   } else {
    // 4 columns
    snprintf(line, sizeof(line), "%-23s ", fno.fname);
    if ( ( (fno.fattrib & AM_HID) != AM_HID) || a_flag) {
     if ( (++column) >= 4) {
      strncat(line, "\n", sizeof(line)-1);
      column = 0;
     }
    }
   }
  }
  if ( ( (fno.fattrib & AM_HID) != AM_HID) || a_flag) {
   FS_type((uint8_t*)line, strlen(line));
  }
  /* Search for next item */
  fr = f_findnext(&dj, &fno);
 }
 if (!l_flag && column != 0) {
  FS_cr();
 }

 f_closedir(&dj);
}

/**
 *  @brief
 *      Change the working directory.
 */
void FS_cd() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;

  fr = f_chdir(line);
  if (fr != FR_OK) {
   strcpy(line, "Err: directory not found");
   FS_type((uint8_t*)line, strlen(line));
   break;
  }
 }
}


/**
 *  @brief
 *      Print working directory
 */
void FS_pwd() {
 FRESULT fr;     /* FatFs return code */

 FS_cr();
 fr = f_getcwd(line, sizeof(line));  /* Get current directory path */
 if (fr == FR_OK) {
  FS_type((uint8_t*)line, strlen(line));
 } else {
  strcpy(line, "Err: no working directory");
  FS_type((uint8_t*)line, strlen(line));
 }
}


/**
 *  @brief
 *      Make directories
 */
void FS_mkdir() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;

  fr = f_mkdir(line);  /* create directory */
  if (fr != FR_OK) {
   FS_type((uint8_t*)line, strlen(line));
   strcpy(line, ": can't create directory ");
   FS_type((uint8_t*)line, strlen(line));
  }
 }
}


/**
 *  @brief
 *      Remove files or directories
 */
void FS_rm() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;

  fr = f_unlink(line);  /* remove file or directory */
  if (fr != FR_OK) {
   FS_type((uint8_t*)line, strlen(line));
   strcpy(line, ": can't remove file or directory  ");
   FS_type((uint8_t*)line, strlen(line));
  }
 }
}


/**
 *  @brief
 *      Change file mode bits
 */
void FS_chmod() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;
 uint8_t param = false;

 BYTE attr = 0;
 BYTE mask = 0;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   if (!param) {
    line[0] = 0;
   }
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;
  if (! strcmp (line, "=r")) {
   attr = AM_RDO;
   mask = AM_RDO | AM_SYS | AM_HID | AM_RDO;
  } else if (! strcmp (line, "=w")) {
   attr = AM_SYS | AM_HID;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=a")) {
   attr = AM_ARC | AM_SYS | AM_HID | AM_RDO;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=rw")) {
   attr = 0;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=ra")) {
   attr = AM_RDO | AM_ARC;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=wa")) {
   attr = AM_SYS | AM_ARC;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=rwa")) {
   attr = AM_ARC;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else if (! strcmp (line, "=")) {
   attr = AM_SYS | AM_HID | AM_RDO;
   mask = AM_ARC | AM_RDO | AM_HID | AM_SYS ;
  } else {
   param = true;
  }
 }

 fr = f_chmod(line, attr, mask);  /* remove file or directory */
 if (fr != FR_OK) {
  FS_type((uint8_t*)line, strlen(line));
  strcpy(line, ": can't change mode");
  FS_type((uint8_t*)line, strlen(line));
 }
}


/**
 *  @brief
 *      Change file timestamps or create files
 */
void FS_touch() {
 FRESULT fr;     /* FatFs return code */
 FIL fil;        /* File object */
 uint8_t *str = NULL;
 int count = 1;
 tmElements_t tm;

 memset(&fno, 0, sizeof(fno));

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;

  RTC.read(tm);

  fno.fdate = (WORD)(
    ((tm.Year + 20) << 9) |
    tm.Month << 5 |
    tm.Day);
  fno.ftime = (WORD)(
    tm.Hour << 11 |
    tm.Minute << 5 |
    tm.Second / 2U);

  // check for file existence
  if (f_stat(line, NULL) == FR_NO_FILE) {
   // file does not exist -> create
   fr = f_open(&fil, line, FA_CREATE_NEW | FA_WRITE);
   if (fr == FR_OK) {
    fr = f_close(&fil);
   } else {
    FS_type((uint8_t*)line, strlen(line));
    strcpy(line, ": can't create file");
    FS_type((uint8_t*)line, strlen(line));
   }
  } else {
   // file exists
   fr = f_utime(line, &fno);  /* create directory */
   if (fr != FR_OK) {
    FS_type((uint8_t*)line, strlen(line));
    strcpy(line, ": can't update timestamps ");
    FS_type((uint8_t*)line, strlen(line));
   }
  }
 }
}


/**
 *  @brief
 *      Move (rename) files
 */
void FS_mv() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;
 int param = 0;

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }

  param++;
  if (param == 1) {
   memcpy(path, str, count);
   path[count] = 0;
   continue;
  } else if (param == 2) {
   memcpy(line, str, count);
   line[count] = 0;
  } else {
   ;
  }

 }

 if (param == 2) {
  fr = f_rename (path, line);  /* move file or directory */
  if (fr != FR_OK) {
   FS_type((uint8_t*)path, strlen(path));
   strcpy(path, ": can't move/rename file or directory  ");
   FS_type((uint8_t*)path, strlen(path));
  }
 } else {
  strcpy(path, "Wrong number of parameters");
  FS_type((uint8_t*)path, strlen(path));
 }
}


/**
 *  @brief
 *      Copy files
 */
void FS_cp() {
 FRESULT fr;     /* FatFs return code */
 uint8_t *str = NULL;
 int count = 1;
 UINT rd_count, wr_count;
 int param = 0;
 FIL fil_src; /* File object */
 FIL fil_dest; /* File object */

 FS_cr();

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   // no more tokens
   break;
  }

  param++;
  if (param == 1) {
   memcpy(path, str, count);
   path[count] = 0;
   continue;
  } else if (param == 2) {
   memcpy(line, str, count);
   line[count] = 0;
  } else {
   ;
  }

 }

 if (param == 2) {
  fr = f_open(&fil_src, path, FA_READ);
  if (fr == FR_OK) {
   fr = f_open(&fil_dest, line, FA_CREATE_ALWAYS | FA_WRITE);
   if (fr == FR_OK) {
    // copy the file
    while (!f_eof(&fil_src)) {
     fr = f_read(&fil_src, mkfs_scratch, SCRATCH_SIZE, &rd_count);
     if (fr != FR_OK) {
      strcpy(path, "Read error");
      FS_type((uint8_t*)path, strlen(path));
      break;
     }
     fr = f_write(&fil_dest, mkfs_scratch, rd_count, &wr_count);
     if (fr != FR_OK || rd_count != wr_count) {
      strcpy(path, "Write error");
      FS_type((uint8_t*)path, strlen(path));
      break;
     }
    }
    f_close(&fil_src);
    f_close(&fil_dest);
   } else {
    // open destination failed
    FS_type((uint8_t*)line, strlen(line));
    strcpy(line, ": can't create file");
    FS_type((uint8_t*)line, strlen(line));
    f_close(&fil_src);
   }
  } else {
   // open source failed
   FS_type((uint8_t*)path, strlen(path));
   strcpy(path, ": file not found");
   FS_type((uint8_t*)path, strlen(path));
  }
 } else {
  strcpy(path, "Wrong number of parameters");
  FS_type((uint8_t*)path, strlen(path));
 }
}


/**
 *  @brief
 *      Report file system disk space usage (1 KiB blocks)
 */
void FS_df() {
 FRESULT fr;     /* FatFs return code */
 FATFS *fatfs;
 DWORD nclst;

 FS_cr();

 fr = f_getfree("", &nclst, &fatfs);  /* Get current directory path */
 if (fr == FR_OK) {
  snprintf(line, sizeof(line), "%lu KiB (%lu clusters) total %lu KiB",
    nclst * (fatfs->csize)/2, nclst, (fatfs->n_fatent - 2) * (fatfs->csize)/2);
  FS_type((uint8_t*)line, strlen(line));
 } else {
  strcpy(line, "Err: no volume");
  FS_type((uint8_t*)line, strlen(line));
 }
}


/**
 *  @brief
 *      Print time and date
 */
void FS_date() {
 time_t time;
 struct tm* tm_p;
 uint8_t *str = NULL;
 int count = 1;
 uint8_t param = false;

 while (true) {
  // get tokens till end of line
  FS_token(&str, &count);
  if (count == 0) {
   if (!param) {
    line[0] = 0;
   }
   break;
  }
  memcpy(line, str, count);
  line[count] = 0;
 }

 FS_cr();

 time = RTC.get();
 tm_p = localtime(&time);

 strftime(line, sizeof(line), "%c", tm_p);

 FS_type((uint8_t*)line, strlen(line));
 FS_cr();
}


/**
 *  @brief
 *      Mount drive
 */
void FS_mount() {
 FRESULT fr;     /* FatFs return code */

 fr = f_mount(&FatFs_SD, "", 1);
 if (fr != FR_OK) {
  strcpy(line, "Can't mount drive");
  FS_type((uint8_t*)line, strlen(line));
 }
}


/**
 *  @brief
 *      Umount rive
 */
void FS_umount() {
 FRESULT fr;     /* FatFs return code */

 fr = f_unmount("");
 if (fr != FR_OK) {
  strcpy(line, "Can't unmount drive");
  FS_type((uint8_t*)line, strlen(line));
 }
}

/**
 *  @brief
 *      Creates an FAT/exFAT volume on the logical drive.
 */
void FS_mkfs() {
 FRESULT fr = FR_OK;     /* FatFs return code */

 FS_cr();
 fr = f_mkfs("", 0, mkfs_scratch, SCRATCH_SIZE);
 if (fr != FR_OK) {
  strcpy(line, "Can't create filesystem.");
  FS_type((uint8_t*)line, strlen(line));
 }
}


int FS_FIL_size(void) {
 return(sizeof(FIL));
}

int FS_FATFS_size(void) {
 return(sizeof(FATFS));
}

int FS_DIR_size(void) {
 return(sizeof(DIR));
}

int FS_FILINFO_size(void) {
 return(sizeof(FILINFO));
}

int FS_FILINFO_fsize(void) {
 return(0);
}

int FS_FILINFO_fdate(void) {
 return(sizeof(FSIZE_t));
}

int FS_FILINFO_ftime(void) {
 return(sizeof(FSIZE_t)+sizeof(WORD));
}

int FS_FILINFO_fattrib(void) {
 return(sizeof(FSIZE_t)+sizeof(WORD)+sizeof(WORD));
}

int FS_FILINFO_fname(void) {
#if _USE_LFN != 0
 return(sizeof(FSIZE_t)+sizeof(WORD)+sizeof(WORD)+sizeof(BYTE)+13*sizeof(BYTE));
#else
 return(sizeof(FSIZE_t)+sizeof(WORD)+sizeof(WORD)+sizeof(BYTE));
#endif
}

int FS_FILINFO_altname(void) {
 return(sizeof(FSIZE_t)+sizeof(WORD)+sizeof(WORD)+sizeof(BYTE));
}

int FS_f_eof(FIL* fp) {
 return f_eof(fp);
}

FSIZE_t FS_f_size(FIL* fp) {
 return f_size(fp);
}

int FS_f_error(FIL* fp) {
 return f_error(fp);
}

/**
 *  @brief
 *     Reads the next character from stream and returns it as an unsigned char cast to an
       int, or EOF on end of file or error.
 *  @param[in]
 *      fp   File object structure
 *  @return
 *     unsigned char cast to an int, or EOF on end of file or error.
 */
int FS_getc(FIL* fp) {
 int buffer = ' ';
 unsigned int count;

 if (f_write(fp, &buffer, 1, &count) != FR_OK) {
  return -1;
 }

 if (count != 1) {
  return -2;
 }
 return buffer;
}

void FS_evaluate (uint8_t *str, int count) {
  forth_evaluate((char*)str, count);
}

void FS_type(uint8_t *str, int count) {
  forth_pushda((uint32_t)str);
  forth_pushda((uint32_t)count);
  forth_execute_xt(type_xt);
}

// C interface to: cr ( -- )
void FS_cr() {
  forth_execute_xt(cr_xt);
}

// C interface to: token ( -- c-addr len )
void FS_token(uint8_t **str, int *count) {
  forth_execute_xt(token_xt);
  *count = (int)forth_popda();
  *str = (uint8_t*)forth_popda();
}

// C interface to: accept ( c-addr maxlength -- length )
void FS_accept(uint8_t *str, int *count) {
  forth_pushda((uint32_t)str);
  forth_pushda((uint32_t)*count);
  forth_execute_xt(accept_xt);
  *count = (uint32_t)forth_popda();
}

