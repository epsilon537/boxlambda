const char fs_fs[] =  R"fs_fs(
\ path is the principle working buffer for filesystem paths.
256 buffer: path

\ path2 is the secondary working buffer for filessystem paths (f_rename / f_findfirst).
256 buffer: path2

\ Put a pathname string of max 255 bytes in the path object.
\ ( c-addr len  -- )
: str>path path -rot cstr ;

\ Read string from input stream into path.
\ ( "path" -- )
: path" path cstr" ;

\ Get pathname string from path object
\ ( -- c-addr len )
: path>str path s0>s ;

\ Put a pathname string of max 255 bytes in the path2 object.
\ ( c-addr len path/path2/pattern -- )
: str>path2 path2 -rot cstr ;

\ Read string from input stream into path2.
\ ( "path" -- )
: path2" path2 cstr" ;

\ Get pathname string from path2 object
\ ( -- c-addr len )
: path2>str path2 s0>s ;

\ FILINFO object, populated by directory operations below.
/FILINFO buffer: filinfo

\ Retrieve file size from filinfo object.
\ ( filinfo -- fsize )
: .fsize
  fsize+ + @
;

\ Retrieve date from filinfo object.
\ ( filinfo -- dd mm yy )
: .fdate
  fdate+ + @ \ ( yymmdd )
  dup 9 rshift 127 and swap \ ( yy yymmdd )
  dup 5 rshift 15 and -rot \ ( mm yy yymmdd )
  31 and -rot \ ( dd mm yy )
;

\ Retrieve time from filinfo object.
\ ( filinfo -- hh mm ss )
: .ftime
  ftime+ + @ \ ( hhmmss )
  dup 11 rshift 31 and swap \ ( hh hhmmss )
  dup 5 rshift 62 and -rot \ ( mm hh hhmmss )
  31 and shl -rot \ ( ss mm hh )
;

\ Retrieve attributes from filinfo object.
\ ( filinfo -- attrib )
: .fattrib
  fattrib+ + c@
;

\ Retrieve name from filinfo object.
\ ( filinfo -- c-addr len )
: .fname
  fname+ + s0>s
;

\ To create a fil object:
\ create fil /FIL allot

\ To create a dir object:
\ create dir /DIR allot

\
\ File Access:
\

\ Open the file specified in the path object.
\ Mode values:
\ FA_READ, FA_WRITE, FA_OPEN_EXISTING, FA_CREATE_NEW, FA_CREATE_ALWAYS,
\ FA_OPEN_ALWAYS, FA_OPEN_APPEND

\ ( fil mode -- ior )
: f_open
  path swap \ ( file path mode )
  fs_f_open
;

\ Close file
\ ( fil -- ior )
: f_close
  fs_f_close
;

\ Internal variable used to hold a numbytes value.
0 variable (fs_nbytes)

\ Read n bytes from file into buffer
\ ( fil buf numbytes -- numbytes ior )
: f_read
  (fs_nbytes)    \ ( fp buf btr *br )
  fs_f_read      \ ( ior )
  (fs_nbytes) @ swap \ ( nbytes ior )
;

\ Write n bytes from buffer into file.
\ ( fil buf numbytes -- numbytes ior )
: f_write
  (fs_nbytes)        \ ( fp buf btw *bw )
  fs_f_write         \ ( ior )
  (fs_nbytes) @ swap \ ( nbytesd ior )
;

\ Moves the file read/write pointer of an open file. Can also be used to expand
\ the file size (cluster pre-allocation).
\ ( fil offset -- ior )
: f_lseek
  fs_f_lseek
;

\ Truncate file size to current read/write pointer.
\ ( fil -- ior )
: f_truncate
  fs_f_truncate
;

\ Flush cached data
\ ( fil -- ior )
: f_sync
  fs_f_sync
;

\ Prepare or allocate a contiguous data area to the file.
\ ( fil size opt -- ior )
: f_expand
  fs_f_expand
;

\ Read a string from the file
\ ( fil len buf -- buf )
: f_gets
  fs_f_gets
;

\ Write a character to the file
\ ( file c -- ior )
: f_putc
  fs_f_putc
;

\ Write a string to the file
\ ( fil str -- ior )
: f_puts
  fs_f_puts
;

\ Get current read/write pointer
\ ( fil -- rwptr )
: f_tell
  fs_f_tell
;

\ Test for end of file.
\ ( fil -- flag )
: f_eof
  fs_f_eof
;

\ Get file size
\ ( fil -- size )
: f_size
  fs_f_size
;

\ Test for an error
\ ( fil -- flag )
: f_error
  fs_f_error
;

\ Print the IO return code string.
\ ( ior -- )
: .ior
  case
    FR_OK of ." FR_OK" endof
    FR_DISK_ERR of ." FR_DISK_ERR" endof
    FR_INT_ERR of ." FR_INT_ERR" endof
    FR_NOT_READY of ." FR_NOT_READY" endof
    FR_NO_FILE of ." FR_NO_FILE" endof
    FR_NO_PATH of ." FR_NO_PATH" endof
    FR_INVALID_NAME of ." FR_INVALID_NAME" endof
    FR_DENIED of ." FR_DENIED" endof
    FR_EXIST of ." FR_EXIST" endof
    FR_INVALID_OBJECT of ." FR_INVALID_OBJECT" endof
    FR_WRITE_PROTECTED of ." FR_WRITE_PROTECTED" endof
    FR_INVALID_DRIVE of ." FR_INVALID_DRIVE" endof
    FR_NOT_ENABLED of ." FR_NOT_ENABLED" endof
    FR_NO_FILESYSTEM of ." FR_NO_FILESYSTEM" endof
    FR_MKFS_ABORTED of ." FR_MKFS_ABORTED" endof
    FR_TIMEOUT of ." FR_TIMEOUT" endof
    FR_LOCKED of ." FR_LOCKED" endof
    FR_NOT_ENOUGH_CORE of ." FR_NOT_ENOUGH_CORE" endof
    FR_TOO_MANY_OPEN_FILES of ." FR_TOO_MANY_OPEN_FILES" endof
    FR_INVALID_PARAMETER of ." FR_INVALID_PARAMETER" endof
    ." Unkown IOR"
  endcase
;

\
\ Directory Access:
\

\ Open the directory specified in the path object.
\ ( dir -- ior )
: f_opendir
  path
  fs_f_opendir
;

\ Close directory
\ ( dir -- ior )
: f_closedir
  fs_f_closedir
;

\ Read directory entry into the filinfo object.
\ ( dir -- ior )
: f_readdir
  filinfo
  fs_f_readdir
;

\ Rewind directory
\ ( dir -- ior )
: f_rewind
  0 ( dir 0 )
  fs_f_readdir
;

\ Open the directory specified in path and read first item patching pattern specified in path2.
\ Put result in filinfo object.
\ ( dir -- ior )
: f_findfirst
  filinfo path path2 ( dir nfo path pattern )
  fs_f_findfirst
;

\ Find next entry matching pattern.
\ Put result in filinfo object.
\ ( dir -- ior )
: f_findnext
  filinfo
  fs_f_findnext
;

\
\ File and Directory Management:
\

\ Check existence of file or directory specified in path. Put result in filinfo.
\ ( -- ior )
: f_stat
  path
  filinfo
  fs_f_stat
;

\ Remove file or subdirectory specified in path.
\ ( -- ior )
: f_unlink
  path
  fs_f_unlink
;

\ Rename/move fil/dir in path to path2.
\ ( -- ior )
: f_rename
  path2
  path
  fs_f_rename
;

\ Change attribute of file or directory specified in path.
\ Attribute values:
\ AM_RDO, AM_ARC, AM_SYS, AM_HID
\ ( attr mask -- ior )
: f_chmod
  path -rot
  fs_f_chmod
;

\ Change timestamp of file or subdirectory specified in path to value specified in filinfo.
\ ( -- ior )
: f_utime
  path
  filinfo
  fs_f_utime
;

\ Make directory specified in path.
\ ( -- ior )
: f_mkdir
  path
  fs_f_mkdir
;

\ Change current directory to directory specified in path.
\ ( -- ior )
: f_chdir
  path
  fs_f_chdir
;

\ Get current working directory into path.
\ ( -- c-addr len ior )
: f_getcwd
  path 255 fs_f_getcwd ( ior )
  path>str ( ior c-addr len )
  rot ( len c-addr ior )
;

\
\ Volume Management and System Configuration:
\

\ Get total and free bytes on volume.
\ ( -- tot free ior )
: f_getfree
  s0"
  fs_f_getfree
;
)fs_fs";
