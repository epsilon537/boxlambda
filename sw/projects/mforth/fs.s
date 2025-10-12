/**
 *  BoxLambda port of Mecrisp Cube's FS module.
 *  Ruben Lysens / Epsilon537.
 *
 *  Original header:
 *
 *  @brief
 *      FAT filesystem for Secure Digital Memory Card. *
 *
 *      Block words.
 *      FS words like include.
 *      Some file tools like GNU tools e.g. ls, pwd, cat. For details see fs.c.
 *      Forth API to the FAT FS functions.
 *  @file
 *      fs.s
 *  @author
 *      Peter Schmid, peter@spyr.ch
 *  @date
 *      2020-07-17
 *  @remark
 *      Language: ARM Assembler, STM32CubeIDE GCC
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

// Forth words which call C functions and which themselves call Forth words
// ************************************************************************

// C Interface to some Forth Words
//********************************

// These functions call Forth words. They need a data stack SPS and
// top of stack (TOS).

.if 0 // FIXME: port this. It is used by vi
// catch_evaluate
//***************

// If there is an error in evaluate the interpreter throws an exceptions and
// aborts the calling words. Mecrisp provides a quit hook which is used to
// catch this abort.
// uint64_t FS_catch_evaluate(uint64_t forth_stack, uint8_t* str, int count);
.global		FS_catch_evaluate
FS_catch_evaluate:
	push 	{r4-r7, lr}
	movs	tos, r0			// get tos
	movs	psp, r1			// get psp
	pushdatos
	movs	tos, r2			// str
	pushdatos
	movs	tos, r3			// count
	ldr		r1, =quit_evaluate	// change quit hook
	ldr 	r0, =hook_quit
	str		r1, [r0]
	ldr		r0, =EvaluateSP	// save stackpointer
	str		sp, [r0]
	ldr		r1, =0			// successful state
	ldr 	r0, =EvaluateState
	str		r1, [r0]
	bl		evaluate
	ldr		r1, =quit_innenschleife	// restore quit hook
	ldr 	r0, =hook_quit
	str		r1, [r0]
	movs	r0, tos			// update tos
	movs	r1, psp			// update psp
	pop		{r4-r7, pc}

quit_evaluate:
	ldr		r1, =1			// error state
	ldr 	r0, =EvaluateState
	str		r1, [r0]
	ldr		r1, =quit_innenschleife	// restore quit hook
	ldr 	r0, =hook_quit
	str		r1, [r0]
	ldr		r0, =EvaluateSP	// restore stackpointer
	ldr		sp, [r0]
	movs	r0, tos			// update tos
	movs	r1, psp			// update psp
	pop		{r4-r7, pc}
.endif


// FAT stdin/stdout user variables
// *******************************

@------------------------------------------------------------------------------
  Definition Flag_visible, "stdin" @ ( -- addr )
  // user variable user_stdin
@------------------------------------------------------------------------------
fs_stdin:
	push	{lr}
	pushdatos
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	adds	tos, r0, #user_stdin
	pop		{pc}

@------------------------------------------------------------------------------
  Definition Flag_visible, "stdout" @ ( -- addr )
  // user variable user_stdout
@------------------------------------------------------------------------------
fs_stdout:
	push	{lr}
	pushdatos
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	adds	tos, r0, #user_stdout
	pop		{pc}

@------------------------------------------------------------------------------
  Definition Flag_visible, "stderr" @ ( -- addr )
  // user variable user_std_err
@------------------------------------------------------------------------------
fs_stderr:
	push	{lr}
	pushdatos
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	adds	tos, r0, #user_stderr
	pop		{pc}


@------------------------------------------------------------------------------
  Definition Flag_visible, "fs-emit" @ ( c -- )
fs_emit:
@------------------------------------------------------------------------------
	push	{lr}
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	ldr		r1, [r0, #user_stdout]	// file descriptor
	movs	r0, tos					// char
	drop
	bl		f_putc
	pop		{pc}


@------------------------------------------------------------------------------
  Definition Flag_visible, "fs-emit?" @ ( --  ?)
fs_qemit:
@------------------------------------------------------------------------------
	push	{lr}
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	ldr		r1, [r0, #user_stdout]	// file descriptor
	bl		FS_f_eof
	cmp		r0, #0
	beq		1f
	ldr		r0, =0
	b		2f
1:
	ldr		r0, =-1	// true
2:
	pushdatos
	movs	tos, r0
	pop		{pc}


@------------------------------------------------------------------------------
  Definition Flag_visible, "fs-key" @ ( -- c )
fs_key:
@------------------------------------------------------------------------------
	push	{lr}
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	ldr		r0, [r0, #user_stdin]	// file descriptor
	bl		FS_getc
	pushdatos
	mov		tos, r0
	pop		{pc}


@------------------------------------------------------------------------------
  Definition Flag_visible, "fs-key?" @ (  -- ? )
fs_qkey:
@------------------------------------------------------------------------------
	push	{lr}
	ldr		r0, =0	// current task xTaskToQuery = 0
	mov		r1, r0	// index
	bl		pvTaskGetThreadLocalStoragePointer
	ldr		r1, [r0, #user_stdin]	// file descriptor
	bl		FS_f_eof
	cmp		r0, #0
	beq		1f
	ldr		r0, =0
	b		2f
1:
	ldr		r0, =-1	// true
2:
	pushdatos
	movs	tos, r0
	pop		{pc}



// FAT FS datastructures
// *********************

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "/FIL"
		@ ( -- u ) Gets the FIL structure size
// int FS_FIL_size(void)
@ -----------------------------------------------------------------------------
slashFIL:
	push	{lr}
	pushdatos
	bl		FS_FIL_size
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "/FATFS"
		@ ( -- u ) Gets the FATFS structure size
// int FS_FATFS_size(void)
@ -----------------------------------------------------------------------------
slashFATFS:
	push	{lr}
	pushdatos
	bl		FS_FATFS_size
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "/DIR"
		@ ( -- u ) Gets the DIR structure size
// int FS_FIL_size(void)
@ -----------------------------------------------------------------------------
slashDIR:
	push	{lr}
	pushdatos
	bl		FS_DIR_size
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "/FILINFO"
		@ ( -- u ) Gets the FIL structure size
// int FS_FILINFO_size(void)
@ -----------------------------------------------------------------------------
slashFILINFO:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_size
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "fsize+"
		@ ( -- u ) Gets the FILINFO structure fsize offset
// int FS_FILINFO_fsize(void)
@ -----------------------------------------------------------------------------
fsizeplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_fsize
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "fdate+"
		@ ( -- u ) Gets the FILINFO structure fdate offset
// int FS_FILINFO_fdate(void)
@ -----------------------------------------------------------------------------
fdateplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_fdate
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "ftime+"
		@ ( -- u ) Gets the FILINFO structure ftime offset
// int FS_FILINFO_ftime(void)
@ -----------------------------------------------------------------------------
ftimeplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_ftime
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "fattrib+"
		@ ( -- u ) Gets the FILINFO structure fattrib offset
// int FS_FILINFO_fattrib(void)
@ -----------------------------------------------------------------------------
fattribplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_fattrib
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "fname+"
		@ ( -- u ) Gets the FILINFO structure fname offset
// int FS_FILINFO_fname(void)
@ -----------------------------------------------------------------------------
fnameplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_fname
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "altname+"
		@ ( -- u ) Gets the FILINFO structure altname offset
// int FS_FILINFO_altname(void)
@ -----------------------------------------------------------------------------
altnameplus:
	push	{lr}
	pushdatos
	bl		FS_FILINFO_altname
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_READ"
		@ ( -- u ) Gets the Mode Flag FA_READ
@ -----------------------------------------------------------------------------
FA_READ:
	push	{lr}
	pushdatos
	movs	tos, #0x01
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_WRITE"
		@ ( -- u ) Gets the Mode Flag FA_WRITE
@ -----------------------------------------------------------------------------
FA_WRITE:
	push	{lr}
	pushdatos
	movs	tos, #0x02
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_OPEN_EXISTING"
		@ ( -- u ) Gets the Mode Flag FA_OPEN_EXISTING
@ -----------------------------------------------------------------------------
FA_OPEN_EXISTING:
	push	{lr}
	pushdatos
	movs	tos, #0x00
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_CREATE_NEW"
		@ ( -- u ) Gets the Mode Flag FA_CREATE_NEW
@ -----------------------------------------------------------------------------
FA_CREATE_NEW:
	push	{lr}
	pushdatos
	movs	tos, #0x04
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_CREATE_ALWAYS"
		@ ( -- u ) Gets the Mode Flag FA_CREATE_ALWAYS
@ -----------------------------------------------------------------------------
FA_CREATE_ALWAYS:
	push	{lr}
	pushdatos
	movs	tos, #0x08
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_OPEN_ALWAYS"
		@ ( -- u ) Gets the Mode Flag FA_OPEN_ALWAYS
@ -----------------------------------------------------------------------------
FA_OPEN_ALWAYS:
	push	{lr}
	pushdatos
	movs	tos, #0x10
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "FA_OPEN_APPEND"
		@ ( -- u ) Gets the Mode Flag FA_OPEN_APPEND
@ -----------------------------------------------------------------------------
FA_OPEN_APPEND:
	push	{lr}
	pushdatos
	movs	tos, #0x30
	pop		{pc}


// File Access
// ***********

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_open"
		@  ( adr cadr u -- u )  opens a file.
// FRESULT f_open (
//  FIL* fp,           /* [OUT] Pointer to the file object structure */
//  const TCHAR* path, /* [IN] File name */
//  BYTE mode          /* [IN] Mode flags */
// );
@ -----------------------------------------------------------------------------
fs_f_open:
	push	{lr}
	movs	r2, tos		// mode
	drop
	movs	r1, tos		// path
	drop
	movs	r0, tos		// fp
	bl		f_open
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_close"
		@  ( adr -- u )  closes a file.
// FRESULT f_close (
//  FIL* fp     /* [IN] Pointer to the file object */
// );
@ -----------------------------------------------------------------------------
fs_f_close:
	push	{lr}
	movs	r0, tos		// fp
	bl		f_close
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_read"
		@  ( adr len adr -- u )  Read data from the file.
// FRESULT f_read (
//   FIL* fp,     /* [IN] File object */
//   void* buff,  /* [OUT] Buffer to store read data */
//   UINT btr,    /* [IN] Number of bytes to read */
//   UINT* br     /* [OUT] Number of bytes read */
// );
@ -----------------------------------------------------------------------------
fs_f_read:
	push	{lr}
	movs	r3, tos		// br
	drop
	movs	r2, tos		// btr
	drop
	movs	r1, tos		// buff
	drop
	movs	r0, tos		// fp
	bl		f_read
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_write"
		@  ( adr len adr -- u )  writes data to a file
// FRESULT f_write (
//   FIL* fp,          /* [IN] Pointer to the file object structure */
//   const void* buff, /* [IN] Pointer to the data to be written */
//   UINT btw,         /* [IN] Number of bytes to write */
//   UINT* bw          /* [OUT] Pointer to the variable to return number of bytes written */
// );
@ -----------------------------------------------------------------------------
fs_f_write:
	push	{lr}
	movs	r3, tos		// bw
	drop
	movs	r2, tos		// btw
	drop
	movs	r1, tos		// buff
	drop
	movs	r0, tos		// fp
	bl		f_write
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_lseek"
		@  ( adr len adr -- u )  Move read/write pointer, Expand size
// FRESULT f_lseek (
//   FIL*    fp,  /* [IN] File object */
//   FSIZE_t ofs  /* [IN] File read/write pointer */
// );
@ -----------------------------------------------------------------------------
fs_f_lseek:
	push	{lr}
	movs	r1, tos		// ofs
	drop
	movs	r0, tos		// fp
	bl		f_lseek
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_truncate"
		@  ( adr len adr -- u )  Truncate file size
// FRESULT f_truncate (
//   FIL* fp     /* [IN] File object */
// );
@ -----------------------------------------------------------------------------
fs_f_truncate:
	push	{lr}
	movs	r0, tos		// fp
	bl		f_truncate
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_sync"
		@  ( adr len adr -- u )  Read data from the file.
// FRESULT f_sync (
//   FIL* fp     /* [IN] File object */
// );
@ -----------------------------------------------------------------------------
fs_f_sync:
	push	{lr}
	movs	r0, tos		// fp
	bl		f_sync
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_forward"
		@  ( adr len adr -- u )  reads the file data and forward it to the data streaming device.
// FRESULT f_forward (
//   FIL* fp,                        /* [IN] File object */
//   UINT (*func)(const BYTE*,UINT), /* [IN] Data streaming function */
//   UINT btf,                       /* [IN] Number of bytes to forward */
//   UINT* bf                        /* [OUT] Number of bytes forwarded */
// );

@ -----------------------------------------------------------------------------
fs_f_forward:
	push	{lr}
	movs	r3, tos		// bf
	drop
	movs	r2, tos		// btf
	drop
	movs	r1, tos		// func
	drop
	movs	r0, tos		// fp
	bl		f_forward
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_expand"
		@  ( adr len adr -- u )  Allocate a contiguous block to the file
// FRESULT f_expand (
//   FIL*    fp,  /* [IN] File object */
//   FSIZE_t fsz, /* [IN] File size expanded to */
//   BYTE    opt  /* [IN] Allocation mode */
// );
@ -----------------------------------------------------------------------------
fs_f_expand:
	push	{lr}
	movs	r2, tos		// opt
	drop
	movs	r1, tos		// fsz
	drop
	movs	r0, tos		// fp
	bl		f_expand
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_gets"
		@  ( adr len adr -- u )  reads a string from the file.
// TCHAR* f_gets (
//   TCHAR* buff, /* [OUT] Read buffer */
//   int len,     /* [IN] Size of the read buffer */
//   FIL* fp      /* [IN] File object */
// );
@ -----------------------------------------------------------------------------
fs_f_gets:
	push	{lr}
	movs	r2, tos		// fp
	drop
	movs	r1, tos		// len
	drop
	movs	r0, tos		// buff
	bl		f_gets
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_putc"
		@  ( adr len adr -- u )  Write a character
// int f_putc (
//   TCHAR chr,  /* [IN] A character to write */
//   FIL* fp     /* [IN] File object */
// );

@ -----------------------------------------------------------------------------
fs_f_putc:
	push	{lr}
	movs	r1, tos		// fp
	drop
	movs	r0, tos		// chr
	bl		f_putc
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_puts"
		@  ( adr len adr -- u )  Read data from the file.
// int f_puts (
//   const TCHAR* str, /* [IN] String */
//   FIL* fp           /* [IN] File object */
// );
@ -----------------------------------------------------------------------------
fs_f_puts:
	push	{lr}
	movs	r1, tos		// fp
	drop
	movs	r0, tos		// str
	bl		f_puts
	movs	tos, r0
	pop		{pc}


//@ -----------------------------------------------------------------------------
//		Definition Flag_visible, "f_tell"
//		@  ( adr len adr -- u )  Get current read/write pointer
// FSIZE_t f_tell (
//   FIL* fp   /* [IN] File object */
// );
//#define f_tell(fp) ((fp)->fptr)
@ -----------------------------------------------------------------------------
//fs_f_tell:
//	push	{lr}
//	movs	r0, tos		// fp
//	bl		f_tell
//	movs	tos, r0
//	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_eof"
		@  ( adr len adr -- u )  Test for end-of-file
// int f_eof (
//   FIL* fp   /* [IN] File object */
// );
// #define f_eof(fp) ((int)((fp)->fptr == (fp)->obj.objsize))
@ -----------------------------------------------------------------------------
fs_f_eof:
	push	{lr}
	movs	r0, tos		// fp
	bl		FS_f_eof
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_size"
		@  ( adr len adr -- u )  Get size
// FSIZE_t f_size (
//   FIL* fp   /* [IN] File object */
// );
// #define f_size(fp) ((fp)->obj.objsize)
@ -----------------------------------------------------------------------------
fs_f_size:
	push	{lr}
	movs	r0, tos		// fp
	bl		FS_f_size
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_error"
		@  ( adr len adr -- u )  Read data from the file.
// int f_error (
//   FIL* fp   /* [IN] File object */
// );
// #define f_error(fp) ((fp)->err)
@ -----------------------------------------------------------------------------
fs_f_error:
	push	{lr}
	movs	r0, tos		// fp
	bl		FS_f_error
	movs	tos, r0
	pop		{pc}


// Directory Access
// ****************

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_opendir"
		@  ( adr len adr -- u )  Open a directory
// FRESULT f_opendir (
//   DIR* dp,           /* [OUT] Pointer to the directory object structure */
//   const TCHAR* path  /* [IN] Directory name */
// );
@ -----------------------------------------------------------------------------
fs_f_opendir:
	push	{lr}
	movs	r1, tos		// path
	drop
	movs	r0, tos		// dp
	bl		f_opendir
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_closedir"
		@  ( adr len adr -- u )  Close an open directory
// FRESULT f_closedir (
//   DIR* dp     /* [IN] Pointer to the directory object */
// );
@ -----------------------------------------------------------------------------
fs_f_closedir:
	push	{lr}
	movs	r0, tos		// dp
	bl		f_closedir
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_readdir"
		@  ( adr len adr -- u )  Read a directory item
// FRESULT f_readdir (
//   DIR* dp,      /* [IN] Directory object */
//   FILINFO* fno  /* [OUT] File information structure */
// );
@ -----------------------------------------------------------------------------
fs_f_readdir:
	push	{lr}
	movs	r1, tos		// fno
	drop
	movs	r0, tos		// dp
	bl		f_readdir
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_findfirst"
		@  ( adr len adr -- u )  Open a directory and read the first item matched
// FRESULT f_findfirst (
//   DIR* dp,              /* [OUT] Poninter to the directory object */
//   FILINFO* fno,         /* [OUT] Pointer to the file information structure */
//   const TCHAR* path,    /* [IN] Pointer to the directory name to be opened */
//   const TCHAR* pattern  /* [IN] Pointer to the matching pattern string */
// );
@ -----------------------------------------------------------------------------
fs_f_findfirst:
	push	{lr}
	movs	r3, tos		// pattern
	drop
	movs	r2, tos		// path
	drop
	movs	r1, tos		// fno
	drop
	movs	r0, tos		// dp
	bl		f_findfirst
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_findnext"
		@  ( adr len adr -- u )  Read a next item matched
// FRESULT f_findnext (
//   DIR* dp,              /* [IN] Poninter to the directory object */
//   FILINFO* fno          /* [OUT] Pointer to the file information structure */
// );
@ -----------------------------------------------------------------------------
fs_f_findnext:
	push	{lr}
	movs	r1, tos		// fno
	drop
	movs	r0, tos		// dp
	bl		f_findnext
	movs	tos, r0
	pop		{pc}


// File and Directory Management
// *****************************

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_stat"
		@  ( adr len adr -- u )  Check existance of a file or sub-directory
// FRESULT f_stat (
//   const TCHAR* path,  /* [IN] Object name */
//   FILINFO* fno        /* [OUT] FILINFO structure */
// );
@ -----------------------------------------------------------------------------
fs_f_stat:
	push	{lr}
	movs	r1, tos		// fno
	drop
	movs	r0, tos		// path
	bl		f_stat
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_unlink"
		@  ( adr len adr -- u )  Remove a file or sub-directory
// FRESULT f_unlink (
//   const TCHAR* path  /* [IN] Object name */
// );
@ -----------------------------------------------------------------------------
fs_f_unlink:
	push	{lr}
	movs	r0, tos		// path
	bl		f_unlink
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_rename"
		@  ( adr len adr -- u )  Rename/Move a file or sub-directory
// FRESULT f_rename (
//   const TCHAR* old_name, /* [IN] Old object name */
//   const TCHAR* new_name  /* [IN] New object name */
// );
@ -----------------------------------------------------------------------------
fs_f_rename:
	push	{lr}
	movs	r1, tos		// new_name
	drop
	movs	r0, tos		// old_name
	bl		f_rename
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_chmod"
		@  ( adr len adr -- u )  Change attribute of a file or sub-directory
// FRESULT f_chmod (
//   const TCHAR* path, /* [IN] Object name */
//   BYTE attr,         /* [IN] Attribute flags */
//   BYTE mask          /* [IN] Attribute masks */
// );
@ -----------------------------------------------------------------------------
fs_f_chmod:
	push	{lr}
	movs	r2, tos		// mask
	drop
	movs	r1, tos		// attr
	drop
	movs	r0, tos		// path
	bl		f_chmod
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_utime"
		@  ( adr len adr -- u )  Change timestamp of a file or sub-directory
// FRESULT f_utime (
//   const TCHAR* path,  /* [IN] Object name */
//   const FILINFO* fno  /* [IN] Time and data to be set */
// );
@ -----------------------------------------------------------------------------
fs_f_utime:
	push	{lr}
	movs	r1, tos		// fno
	drop
	movs	r0, tos		// path
	bl		f_utime
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_mkdir"
		@  ( adr len adr -- u )  Create a sub-directory
// FRESULT f_mkdir (
//   const TCHAR* path /* [IN] Directory name */
// );
@ -----------------------------------------------------------------------------
fs_f_mkdir:
	push	{lr}
	movs	r0, tos		// path
	bl		f_mkdir
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_chdir"
		@  ( adr -- u )  Change current directory.
// FRESULT f_chdir (
// 	const TCHAR* path	/* Pointer to the directory path */
// )
@ -----------------------------------------------------------------------------
fs_f_chdir:
	push	{lr}
	movs	r0, tos		// buff
	bl		f_chdir
	movs	tos, r0
	pop		{pc}

.if SD_DRIVE == 1
@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_chdrive"
		@  ( adr -- u )  Changes the current drive.
// FRESULT f_chdrive (
//  const TCHAR* path  /* [IN] Logical drive number */
// );
@ -----------------------------------------------------------------------------
fs_f_chdrive:
	push	{lr}
	movs	r0, tos		// path
	bl		f_chdrive
	movs	tos, r0
	pop		{pc}
.endif

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_getcwd"
		@  ( adr len -- u )  Get current directory .
// FRESULT f_getcwd (
// 	TCHAR* buff,	/* Pointer to the directory path */
// 	UINT len		/* Size of path */
// )
@ -----------------------------------------------------------------------------
fs_f_getcwd:
	push	{lr}
	movs	r1, tos		// len
	drop
	movs	r0, tos		// buff
	pushdatos
	bl		f_getcwd
	movs	tos, r0
	pop		{pc}


// Volume Management and System Configuration
// ******************************************

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_mount"
		@  ( adr len adr -- u )  Register/Unregister the work area of the volume
// FRESULT f_mount (
//   FATFS*       fs,    /* [IN] Filesystem object */
//   const TCHAR* path,  /* [IN] Logical drive number */
//   BYTE         opt    /* [IN] Initialization option */
// );
@ -----------------------------------------------------------------------------
fs_f_mount:
	push	{lr}
	movs	r2, tos		// opt
	drop
	movs	r1, tos		// path
	drop
	movs	r0, tos		// fs
	bl		f_mount
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_mkfs"
		@  ( adr len adr -- u )  Create an FAT volume on the logical drive
// FRESULT f_mkfs (
//   const TCHAR*  path,  /* [IN] Logical drive number */
//   const MKFS_PARM* opt,/* [IN] Format options */
//   void*  work,         /* [-]  Working buffer */
//   UINT  len            /* [IN] Size of working buffer */
// );
@ -----------------------------------------------------------------------------
fs_f_mkfs:
	push	{lr}
	movs	r3, tos		// len
	drop
	movs	r2, tos		// work
	drop
	movs	r1, tos		// opt
	drop
	movs	r0, tos		// path
	bl		f_mkfs
	movs	tos, r0
	pop		{pc}


//@ -----------------------------------------------------------------------------
//		Definition Flag_visible, "f_fdisk"
//		@  ( adr len adr -- u )  Create partitions on the physical drive
// FRESULT f_fdisk (
//   BYTE  pdrv,         /* [IN] Physical drive number */
//   const LBA_t ptbl[], /* [IN] Partition map table */
//   void* work          /* [IN] Work area */
// );
@ -----------------------------------------------------------------------------
//fs_f_fdisk:
//	push	{lr}
//	movs	r2, tos		// work
//	drop
//	movs	r1, tos		// ptbl
//	drop
//	movs	r0, tos		// pdrv
//	bl		f_fdisk
//	movs	tos, r0
//	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_getfree"
		@  ( adr len adr -- u )  gets number of the free clusters on the volume.
// FRESULT f_read (
//   FIL* fp,     /* [IN] File object */
//   void* buff,  /* [OUT] Buffer to store read data */
//   UINT btr,    /* [IN] Number of bytes to read */
//   UINT* br     /* [OUT] Number of bytes read */
// );
@ -----------------------------------------------------------------------------
fs_f_getfree:
	push	{lr}
	movs	r3, tos		// br
	drop
	movs	r2, tos		// btr
	drop
	movs	r1, tos		// buff
	drop
	movs	r0, tos		// fp
	bl		f_getfree
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_getlabel"
		@  ( adr len adr -- u )  returns volume label and volume serial number of a volume.
// FRESULT f_getlabel (
//   const TCHAR* path,  /* [IN] Drive number */
//   TCHAR* label,       /* [OUT] Volume label */
//   DWORD* vsn          /* [OUT] Volume serial number */
// );

@ -----------------------------------------------------------------------------
fs_f_getlabel:
	push	{lr}
	movs	r2, tos		// vsn
	drop
	movs	r1, tos		// label
	drop
	movs	r0, tos		// path
	bl		f_getlabel
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "f_setlabel"
		@  ( adr len adr -- u )  Set volume label
// FRESULT f_setlabel (
//   const TCHAR* label  /* [IN] Volume label to be set */
// );
@ -----------------------------------------------------------------------------
fs_f_setlabel:
	push	{lr}
	movs	r0, tos		// label
	bl		f_setlabel
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "get_fattime"
		@  (  -- u )  Gets Time from RTC
// DWORD get_fattime (void)
@ -----------------------------------------------------------------------------
fs_get_fattime:
	push	{lr}
	pushdatos
	bl		get_fattime
	movs	tos, r0
	pop		{pc}


// C String Functions
// ******************

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "strlen"
		@  ( adr -- adr len )  size_t	calculate the length of a C string
// size_t	 strlen (const char *);
@ -----------------------------------------------------------------------------
fs_strlen:
	push	{lr}
	movs	r0, tos		// adr
	pushdatos
	bl		strlen
	movs	tos, r0
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "str0term"
		@  ( cadr len --  )  null-terminated string
// : str0term   + 0 swap c! ;
@ -----------------------------------------------------------------------------
str0term:
	push	{lr}
	ldm 	psp!, {r0}
	adds 	tos, r0
	pushdatos
	movs	tos, #0
	swap
	ldm		psp!, {r0, r1}	@ X is the new TOS after the store completes.
	strb	r0, [tos]		@ Popping both saves a cycle.
	movs 	tos, r1
	pop		{pc}



@ -----------------------------------------------------------------------------
		Definition Flag_visible, ".("
		@  ( "text) --  )
// : .( 41 parse type ;
@ -----------------------------------------------------------------------------
dotklammer:
	push	{lr}
	pushdatos
	movs	tos, #41	// ')'
	bl		parse
	bl		stype
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, ".str\""
		@  ( c-addr "text" -- len ) copy string into buffer
// : .str" 34 parse -rot swap rot 2dup + 0 swap ! move ;
@ -----------------------------------------------------------------------------
dotstrquote:
	push	{lr}
	pushdatos
	movs	tos, #34	// '"'
	bl		parse		// dest source len
	bl		minusrot	// len dest source
	swap				// len source dest
	bl		rot			// source dest len
	ddup				// source dest len dest len
	ldm 	psp!, {r0}
	adds	tos, r0		// +
	pushdaconst	0		// 0
	swap				// swap
	ldm		psp!, {r0, r1}
	str		r0, [tos]
	movs tos, r1		// !
	bl		move
	pop		{pc}

@ -----------------------------------------------------------------------------
		Definition Flag_immediate_compileonly, "s0\""
		@ (  -- c-addr len ) Insert a 0-terminated string-literal
@ -----------------------------------------------------------------------------
	push	{lr}
	ldr		r0, =dotsfuesschen
	pushda	r0
	bl		callkomma
	pushdaconst 34 		//  Das Gänsefüßchen "
	bl		parse
	bl		stringkomma
	pushdaconst	0
	bl		hkomma		// add 0-termination
//	drop				// remove count
	pop		{pc}


// RTC Words
//**********

@ -----------------------------------------------------------------------------
		Definition Flag_visible, "time@"
		@  (  -- u )
// int RTC_getTime(void);
@ -----------------------------------------------------------------------------
timeget:
	push	{lr}
	pushdatos
	bl		RTC_getTime
	movs	tos, r0;
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "time!"
		@  ( u --  )
// void RTC_setTime(int timestamp);
@ -----------------------------------------------------------------------------
timeset:
	push	{lr}
	movs	r0, tos	// timestamp
	drop
	bl		RTC_setTime
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, ".time"
		@  (  --  )
// uint64_t RTC_typeTime(uint64_t forth_stack)
@ -----------------------------------------------------------------------------
typetime:
	push	{r4-r7, lr}
	movs	r0, tos		// get tos
	movs	r1, psp		// get psp
	bl		RTC_typeTime
	movs	tos, r0		// update tos
	movs	psp, r1		// update psp
	pop		{r4-r7, pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "clkdate"
		@  (  -- u )  Year*10000 + Month*100 + Day
// int RTC_getClockDate(void);
@ -----------------------------------------------------------------------------
clkdate:
	push	{lr}
	pushdatos
	bl		RTC_getClockDate
	movs	tos, r0;
	pop		{pc}


@ -----------------------------------------------------------------------------
		Definition Flag_visible, "clktime"
		@  (  -- u ) hour*10000 + minute*100 + second
// int RTC_getClockTime(void);
@ -----------------------------------------------------------------------------
clktime:
	push	{lr}
	pushdatos
	bl		RTC_getClockTime
	movs	tos, r0;
	pop		{pc}


