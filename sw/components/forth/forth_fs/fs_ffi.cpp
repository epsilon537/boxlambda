#include "forth.h"
#include "fs_ffi.h"
#include "ff.h" // Fat FS
#include <stddef.h>

// FAT FS datastructures

void slashFIL() {
  forth_pushda(sizeof(FIL));
}

void slashFATFS() {
  forth_pushda(sizeof(FATFS));
}

void slashDIR() {
  forth_pushda(sizeof(DIR));
}

void slashFILINFO() {
  forth_pushda(sizeof(FILINFO));
}

#define FILINFOPLUS(field) void field##plus() { forth_pushda(offsetof(FILINFO, field)); }

FILINFOPLUS(fsize)
FILINFOPLUS(fdate)
FILINFOPLUS(ftime)
FILINFOPLUS(fattrib)
FILINFOPLUS(fname)

#define FA_FLAG(name) void fa_flag_##name() { forth_pushda(name); }

FA_FLAG(FA_READ)
FA_FLAG(FA_WRITE)
FA_FLAG(FA_OPEN_EXISTING)
FA_FLAG(FA_CREATE_NEW)
FA_FLAG(FA_CREATE_ALWAYS)
FA_FLAG(FA_OPEN_ALWAYS)
FA_FLAG(FA_OPEN_APPEND)

#define AM_ATTR(name) void am_attr_##name() { forth_pushda(name); }

AM_ATTR(AM_RDO) //Read-only
AM_ATTR(AM_ARC) //Archive
AM_ATTR(AM_SYS) //System
AM_ATTR(AM_HID) //Hidden

#define FR_CODE(name) void fr_code_##name() { forth_pushda(name); }

FR_CODE(FR_OK)    /* (0) Succeeded */
FR_CODE(FR_DISK_ERR)   /* (1) A hard error occurred in the low level disk I/O layer */
FR_CODE(FR_INT_ERR)    /* (2) Assertion failed */
FR_CODE(FR_NOT_READY)   /* (3) The physical drive cannot work */
FR_CODE(FR_NO_FILE)    /* (4) Could not find the file */
FR_CODE(FR_NO_PATH)    /* (5) Could not find the path */
FR_CODE(FR_INVALID_NAME)  /* (6) The path name format is invalid */
FR_CODE(FR_DENIED)    /* (7) Access denied due to prohibited access or directory full */
FR_CODE(FR_EXIST)    /* (8) Access denied due to prohibited access */
FR_CODE(FR_INVALID_OBJECT)  /* (9) The file/directory object is invalid */
FR_CODE(FR_WRITE_PROTECTED)  /* (10) The physical drive is write protected */
FR_CODE(FR_INVALID_DRIVE)  /* (11) The logical drive number is invalid */
FR_CODE(FR_NOT_ENABLED)   /* (12) The volume has no work area */
FR_CODE(FR_NO_FILESYSTEM)  /* (13) There is no valid FAT volume */
FR_CODE(FR_MKFS_ABORTED)  /* (14) The f_mkfs() aborted due to any problem */
FR_CODE(FR_TIMEOUT)    /* (15) Could not get a grant to access the volume within defined period */
FR_CODE(FR_LOCKED)    /* (16) The operation is rejected according to the file sharing policy */
FR_CODE(FR_NOT_ENOUGH_CORE)  /* (17) LFN working buffer could not be allocated */
FR_CODE(FR_TOO_MANY_OPEN_FILES) /* (18) Number of open files > FF_FS_LOCK */
FR_CODE(FR_INVALID_PARAMETER) /* (19) Given parameter is invalid */

// File Access

void fs_f_open() {
  BYTE mode = (BYTE)forth_popda();
  const TCHAR *path = (const TCHAR *)forth_popda();
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_open(fp, path, mode);

  forth_pushda(res);
}

void fs_f_close() {
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_close(fp);

  forth_pushda(res);
}

void fs_f_read() {
  UINT *br = (UINT*)forth_popda();  // Number of bytes read.
  UINT btr = (UINT)forth_popda();   // Number of bytes to read.
  void *buf = (void*)forth_popda(); // Buffer to store read data.
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_read(fp, buf, btr, br);

  forth_pushda(res);
}

void fs_f_write() {
  UINT *bw = (UINT*)forth_popda();              // Number of bytes written.
  UINT btw = (UINT)forth_popda();               // Number of bytes to write.
  const void *buf = (const void*)forth_popda(); // Buffer to read from.
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_write(fp, buf, btw, bw);

  forth_pushda(res);
}

void fs_f_lseek() {
  FSIZE_t ofs = (FSIZE_t)forth_popda();
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_lseek(fp, ofs);

  forth_pushda(res);
}

void fs_f_truncate() {
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_truncate(fp);

  forth_pushda(res);
}

void fs_f_sync() {
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_sync(fp);

  forth_pushda(res);
}


void fs_f_expand() {
  BYTE opt = (BYTE)forth_popda();
  FSIZE_t fsz = (FSIZE_t)forth_popda();
  FIL *fp = (FIL *)forth_popda();

  FRESULT res = f_expand(fp, fsz, opt);

  forth_pushda(res);
}

void fs_f_gets() {
  FIL *fp = (FIL *)forth_popda();
  int len = (int)forth_popda();
  TCHAR *buff = (TCHAR *)forth_popda();

  TCHAR *res = f_gets(buff, len, fp);

  forth_pushda((uint32_t)res);
}

void fs_f_putc() {
  FIL *fp = (FIL *)forth_popda();
  TCHAR c = (const TCHAR)forth_popda();

  int res = f_putc(c, fp);

  forth_pushda(res);
}

void fs_f_puts() {
  FIL *fp = (FIL *)forth_popda();
  const TCHAR* s = (const TCHAR *)forth_popda();

  int res = f_puts(s, fp);

  forth_pushda(res);
}

void fs_f_tell() {
  FIL *fp = (FIL *)forth_popda();

  FSIZE_t res = f_tell(fp);

  forth_pushda(res);
}

void fs_f_eof() {
  FIL *fp = (FIL *)forth_popda();

  int res = f_eof(fp);

  forth_pushda(res);
}

void fs_f_size() {
  FIL *fp = (FIL *)forth_popda();

  FSIZE_t res = f_size(fp);

  forth_pushda(res);
}

void fs_f_error() {
  FIL *fp = (FIL *)forth_popda();

  int res = f_error(fp);

  forth_pushda(res);
}

// Directory Access

void fs_f_opendir() {
  const TCHAR* path = (const TCHAR *)forth_popda();
  DIR *dp = (DIR *)forth_popda();

  FRESULT res = f_opendir(dp, path);

  forth_pushda(res);
}

void fs_f_closedir() {
  DIR *dp = (DIR *)forth_popda();

  FRESULT res = f_closedir(dp);

  forth_pushda(res);
}

void fs_f_readdir() {
  FILINFO *nfo = (FILINFO *)forth_popda();
  DIR *dp = (DIR *)forth_popda();

  FRESULT res = f_readdir(dp, nfo);

  forth_pushda(res);
}

void fs_f_findfirst() {
  const TCHAR* pattern = (const TCHAR *)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();
  FILINFO *nfo = (FILINFO *)forth_popda();
  DIR *dp = (DIR *)forth_popda();

  FRESULT res = f_findfirst(dp, nfo, path, pattern);

  forth_pushda(res);
}

void fs_f_findnext() {
  FILINFO *nfo = (FILINFO *)forth_popda();
  DIR *dp = (DIR *)forth_popda();

  FRESULT res = f_findnext(dp, nfo);

  forth_pushda(res);
}

// File and Directory Management

void fs_f_stat() {
  FILINFO *nfo = (FILINFO *)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_stat(path, nfo);

  forth_pushda(res);
}

void fs_f_unlink() {
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_unlink(path);

  forth_pushda(res);
}

void fs_f_rename() {
  const TCHAR* new_name = (const TCHAR *)forth_popda();
  const TCHAR* old_name = (const TCHAR *)forth_popda();

  FRESULT res = f_rename(old_name, new_name);

  forth_pushda(res);
}

void fs_f_chmod() {
  BYTE mask = (const BYTE)forth_popda();
  BYTE attr = (const BYTE)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_chmod(path, attr, mask);

  forth_pushda(res);
}

void fs_f_utime() {
  const FILINFO *nfo = (FILINFO *)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_utime(path, nfo);

  forth_pushda(res);
}

void fs_f_mkdir() {
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_mkdir(path);

  forth_pushda(res);
}

void fs_f_chdir() {
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_chdir(path);

  forth_pushda(res);
}

void fs_f_chdrive() {
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_chdrive(path);

  forth_pushda(res);
}

void fs_f_getcwd() {
  UINT len = (UINT)forth_popda();
  TCHAR* buf = (TCHAR *)forth_popda();

  FRESULT res = f_getcwd(buf, len);

  forth_pushda(res);
}

// Volume Management and System Configuration

void fs_f_mount() {
  BYTE opt = (const BYTE)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();
  FATFS *fs = (FATFS *)forth_popda();

  FRESULT res = f_mount(fs, path, opt);

  forth_pushda(res);
}

void fs_f_mkfs() {
  UINT len = (UINT)forth_popda();
  void* work = (void *)forth_popda();
  const MKFS_PARM* opt = (const MKFS_PARM *)forth_popda();
  const TCHAR* path = (const TCHAR *)forth_popda();

  FRESULT res = f_mkfs(path, opt, work, len);

  forth_pushda(res);
}

// Get total and free bytes for given drive number.
// Free and total values are only returned if res is 0.
// ( drvnumstr -- res [ free tot ])
void fs_f_getfree() {
  const TCHAR* path = (const TCHAR *)forth_popda();
  FATFS *fs;
  DWORD fre_clust;
  DWORD fre_sect=0, tot_sect=0;

  FRESULT res = f_getfree(path, &fre_clust, &fs);

  if (!res) {
    /* Get total sectors and free sectors */
    tot_sect = ((fs->n_fatent - 2) * fs->csize)/2;
    fre_sect = (fre_clust * fs->csize)/2;
  }

  forth_pushda(tot_sect);
  forth_pushda(fre_sect);
  forth_pushda(res);
}

void fs_ffi_init() {
  forth_register_cfun(slashFIL, "/FIL");
  forth_register_cfun(slashFATFS, "/FATFS");
  forth_register_cfun(slashDIR, "/DIR");
  forth_register_cfun(slashFILINFO, "/FILINFO");
  forth_register_cfun(fsizeplus, "fsize+");
  forth_register_cfun(fdateplus, "fdate+");
  forth_register_cfun(ftimeplus, "ftime+");
  forth_register_cfun(fattribplus, "fattrib+");
  forth_register_cfun(fnameplus, "fname+");
  forth_register_cfun(fa_flag_FA_READ, "FA_READ");
  forth_register_cfun(fa_flag_FA_WRITE, "FA_WRITE");
  forth_register_cfun(fa_flag_FA_OPEN_EXISTING, "FA_OPEN_EXISTING");
  forth_register_cfun(fa_flag_FA_CREATE_NEW, "FA_CREATE_NEW");
  forth_register_cfun(fa_flag_FA_CREATE_ALWAYS, "FA_CREATE_ALWAYS");
  forth_register_cfun(fa_flag_FA_OPEN_ALWAYS, "FA_OPEN_ALWAYS");
  forth_register_cfun(fa_flag_FA_OPEN_APPEND, "FA_OPEN_APPEND");
  forth_register_cfun(am_attr_AM_RDO, "AM_RDO");
  forth_register_cfun(am_attr_AM_ARC, "AM_ARC");
  forth_register_cfun(am_attr_AM_SYS, "AM_SYS");
  forth_register_cfun(am_attr_AM_HID, "AM_HID");
  forth_register_cfun(fr_code_FR_OK, "FR_OK");
  forth_register_cfun(fr_code_FR_DISK_ERR, "FR_DISK_ERR");
  forth_register_cfun(fr_code_FR_INT_ERR, "FR_INT_ERR");
  forth_register_cfun(fr_code_FR_NOT_READY, "FR_NOT_READY");
  forth_register_cfun(fr_code_FR_NO_FILE, "FR_NO_FILE");
  forth_register_cfun(fr_code_FR_NO_PATH, "FR_NO_PATH");
  forth_register_cfun(fr_code_FR_INVALID_NAME, "FR_INVALID_NAME");
  forth_register_cfun(fr_code_FR_DENIED, "FR_DENIED");
  forth_register_cfun(fr_code_FR_EXIST, "FR_EXIST");
  forth_register_cfun(fr_code_FR_INVALID_OBJECT, "FR_INVALID_OBJECT");
  forth_register_cfun(fr_code_FR_WRITE_PROTECTED, "FR_WRITE_PROTECTED");
  forth_register_cfun(fr_code_FR_INVALID_DRIVE, "FR_INVALID_DRIVE");
  forth_register_cfun(fr_code_FR_NOT_ENABLED, "FR_NOT_ENABLED");
  forth_register_cfun(fr_code_FR_NO_FILESYSTEM, "FR_NO_FILESYSTEM");
  forth_register_cfun(fr_code_FR_MKFS_ABORTED, "FR_MKFS_ABORTED");
  forth_register_cfun(fr_code_FR_TIMEOUT, "FR_TIMEOUT");
  forth_register_cfun(fr_code_FR_LOCKED, "FR_LOCKED");
  forth_register_cfun(fr_code_FR_NOT_ENOUGH_CORE, "FR_NOT_ENOUGH_CORE");
  forth_register_cfun(fr_code_FR_TOO_MANY_OPEN_FILES, "FR_TOO_MANY_OPEN_FILES");
  forth_register_cfun(fr_code_FR_INVALID_PARAMETER, "FR_INVALID_PARAMETER");
  forth_register_cfun(fs_f_open, "fs_f_open");
  forth_register_cfun(fs_f_close, "fs_f_close");
  forth_register_cfun(fs_f_read, "fs_f_read");
  forth_register_cfun(fs_f_write, "fs_f_write");
  forth_register_cfun(fs_f_lseek, "fs_f_lseek");
  forth_register_cfun(fs_f_truncate, "fs_f_truncate");
  forth_register_cfun(fs_f_sync, "fs_f_sync");
  forth_register_cfun(fs_f_expand, "fs_f_expand");
  forth_register_cfun(fs_f_gets, "fs_f_gets");
  forth_register_cfun(fs_f_puts, "fs_f_putc");
  forth_register_cfun(fs_f_puts, "fs_f_puts");
  forth_register_cfun(fs_f_tell, "fs_f_tell");
  forth_register_cfun(fs_f_eof, "fs_f_eof");
  forth_register_cfun(fs_f_size, "fs_f_size");
  forth_register_cfun(fs_f_error, "fs_f_error");
  forth_register_cfun(fs_f_opendir, "fs_f_opendir");
  forth_register_cfun(fs_f_closedir, "fs_f_closedir");
  forth_register_cfun(fs_f_readdir, "fs_f_readdir");
  forth_register_cfun(fs_f_findfirst, "fs_f_findfirst");
  forth_register_cfun(fs_f_findnext, "fs_f_findnext");
  forth_register_cfun(fs_f_stat, "fs_f_stat");
  forth_register_cfun(fs_f_unlink, "fs_f_unlink");
  forth_register_cfun(fs_f_rename, "fs_f_rename");
  forth_register_cfun(fs_f_chmod, "fs_f_chmod");
  forth_register_cfun(fs_f_utime, "fs_f_utime");
  forth_register_cfun(fs_f_mkdir, "fs_f_mkdir");
  forth_register_cfun(fs_f_chdir, "fs_f_chdir");
  forth_register_cfun(fs_f_chdrive, "fs_f_chdrive");
  forth_register_cfun(fs_f_getcwd, "fs_f_getcwd");
  forth_register_cfun(fs_f_mount, "fs_f_mount");
  forth_register_cfun(fs_f_mkfs, "fs_f_mkfs");
  forth_register_cfun(fs_f_getfree, "fs_f_getfree");
}

