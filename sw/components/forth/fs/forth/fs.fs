const char fs_fs[] =  R"fs_fs(

\ path is the principle working buffer for filesystem paths.
256 buffer: path

\ path2 is the secondary working buffer for filessystem paths (f_rename / f_findfirst).
256 buffer: path2

\ Put a pathname string of max 255 bytes in the path object.
\ ( c-addr len path -- )
: str>path -rot cstr ;

\ Get pathname string from path object
\ ( path -- c-addr len )
: path>str s0>s ;

\ FILINFO global object, populated by directory operations below.
/FILINFO buffer: filinfo

\ 0-initialize
filinfo /FILINFO 0 fill

\
\ filinfo object accessors
\

\ Retrieve file size from global filinfo object.
\ ( -- fsize )
: filinfo.fsize
  filinfo fsize+ + @
;

\ Set date in global filinfo object.
\ ( dd mm yyyy -- )
: filinfo.setfdate
  1980 - $7f and 9 lshift -rot ( yy0000 dd mm )
  $f and 5 lshift -rot ( mm00 yy0000 dd )
  $1f and or or ( yymmdd )
  filinfo fdate+ + !
;

\ Retrieve date from global  filinfo object.
\ ( -- dd mm yyyy )
: filinfo.getfdate
  filinfo fdate+ + @ \ ( yymmdd )
  dup 9 rshift $7f and 1980 + swap \ ( yy yymmdd )
  dup 5 rshift $f and -rot \ ( mm yy yymmdd )
  $1f and -rot \ ( dd mm yy )
;

\ Set time in global filinfo object.
\ ( hh mm ss -- )
: filinfo.setftime
  shr $1f and -rot ( 0000ss hh mm )
  $3f and 5 lshift -rot ( mm00 0000ss hh )
  $1f and #11 lshift ( mm00 0000ss hh0000 )
  or or ( hhmmss )
  filinfo ftime+ + !
;

\ Retrieve time from global filinfo object.
\ ( -- hh mm ss )
: filinfo.getftime
  filinfo ftime+ + @ \ ( hhmmss )
  dup #11 rshift $1f and swap \ ( hh hhmmss )
  dup 5 rshift $3f and swap \ ( hh mm hhmmss )
  $1f and shl \ ( hh mm ss )
;

\ Retrieve attributes from global filinfo object.
\ AM_RDO, AM_ARC, AM_SYS, AM_HID, AM_DIR
\ ( -- attrib )
: filinfo.fattrib filinfo fattrib+ + c@ ;

\ Retrieve name from global filinfo object.
\ ( -- c-addr len )
: filinfo.fname filinfo fname+ + s0>s ;

\ File object pool
64 constant MAX_NUM_OPEN_FILES
MAX_NUM_OPEN_FILES /FIL * constant FILE_POOL_MEM_SZ
create file-pool-memory FILE_POOL_MEM_SZ allot
create file-pool pool-size allot
/FIL file-pool init-pool
file-pool-memory FILE_POOL_MEM_SZ file-pool add-pool

\ Directory object pool
8 constant MAX_NUM_OPEN_DIRS
MAX_NUM_OPEN_DIRS /DIR * constant DIR_POOL_MEM_SZ
create dir-pool-memory DIR_POOL_MEM_SZ allot
create dir-pool pool-size allot
/DIR dir-pool init-pool
dir-pool-memory DIR_POOL_MEM_SZ dir-pool add-pool

\ Exceptions:
: x-fr-disk-err ." FR: (1) A hard error occurred in the low level disk I/O layer" cr ;
: x-fr-int-err ." FR: (2) Assertion failed" cr ;
: x-fr-not-ready ." FR: (3) The physical drive cannot work" cr ;
: x-fr-no-file ." FR: (4) Could not find the file" cr ;
: x-fr-no-path ." FR: (5) Could not find the path" cr ;
: x-fr-invalid-name ." FR: (6) The path name format is invalid" cr ;
: x-fr-denied ." FR: (7) Access denied due to prohibited access or directory full" cr ;
: x-fr-exist ." FR: (8) Access denied due to prohibited access" cr ;
: x-fr-invalid-object ." FR: (9) The file/directory object is invalid" cr ;
: x-fr-write-protected ." FR: (10) The physical drive is write protected" cr ;
: x-fr-invalid-drive ." FR: (11) The logical drive number is invalid" cr ;
: x-fr-not-enabled ." FR: (12) The volume has no work area" cr ;
: x-fr-no-filesystem ." FR: (13) There is no valid FAT volume" cr ;
: x-fr-mkfs-aborted ." FR: (14) The f-mkfs() aborted due to any problem" cr ;
: x-fr-timeout ." FR: (15) Could not get a grant to access the volume within defined period" cr ;
: x-fr-locked ." FR: (16) The operation is rejected according to the file sharing policy" cr ;
: x-fr-not-enough-core ." FR: (17) LFN working buffer could not be allocated" cr ;
: x-fr-too-many-open-files ." FS: (18) Number of open files > FF-FS-LOCK" cr ;
: x-fr-invalid-parameter ." FS: (19) The given parameter is invalid or there is an inconsistent for the volume." cr ;
: x-fr-unknown ." FR: (??) Unknown exception" cr ;

\ ( ior -- xt)
: ior>exception
  case
    FR_OK of 0 endof
    FR_DISK_ERR of ['] x-fr-disk-err endof
    FR_INT_ERR of ['] x-fr-int-err endof
    FR_NOT_READY of ['] x-fr-not-ready endof
    FR_NO_FILE of ['] x-fr-no-file endof
    FR_NO_PATH of ['] x-fr-no-path endof
    FR_INVALID_NAME of ['] x-fr-invalid-name endof
    FR_DENIED of ['] x-fr-denied endof
    FR_EXIST of ['] x-fr-exist endof
    FR_INVALID_OBJECT of ['] x-fr-invalid-object endof
    FR_WRITE_PROTECTED of ['] x-fr-write-protected endof
    FR_INVALID_DRIVE of ['] x-fr-invalid-drive endof
    FR_NOT_ENABLED of ['] x-fr-not-enabled endof
    FR_NO_FILESYSTEM of ['] x-fr-no-filesystem endof
    FR_MKFS_ABORTED of ['] x-fr-mkfs-aborted endof
    FR_TIMEOUT of ['] x-fr-timeout endof
    FR_LOCKED of ['] x-fr-locked endof
    FR_NOT_ENOUGH_CORE of ['] x-fr-not-enough-core endof
    FR_TOO_MANY_OPEN_FILES of ['] x-fr-too-many-open-files endof
    FR_INVALID_PARAMETER of ['] x-fr-invalid-parameter endof
    ['] x-fr-unknown
  endcase
;

\ Check IOR and throw exception if non-zero
\ ( ior -- )
: check-throw-ior ior>exception ?raise ;

\ Print the IO return code string.
\ ( ior -- )
: .ior ior>exception execute ;

\
\ File Access:
\

\ Open the file specified in input string.
\ Mode argument is a combination of following values:
\ FA_READ:Specifies read access to the file. Data can be read from the file.
\ FA_WRITE: Specifies write access to the file. Data can be written to the file. Combine with FA_READ for read-write access.
\ FA_OPEN_EXISTING:	Opens the file. The function fails if the file is not existing. (Default)
\ FA_CREATE_ALWAYS: Creates a new file. If the file is existing, the file is truncated and overwritten.
\ FA_CREATE_NEW: Creates a new file. The function fails if the file is existing.
\ FA_OPEN_ALWAYS: Opens the file. If it is not exist, a new file is created.
\ FA_OPEN_APPEND: Same as FA_OPEN_ALWAYS except the read/write pointer is set end of the file.
\ May throw x-fr-* and x-pool-* exceptions.
\ ( addr len mode -- fil )
: f_open
  -rot path str>path ( mode )
  file-pool allocate-pool >r ( mode )
  r@ path rot ( fil path mode )
  fs_f_open ( ior )
  ?dup if ( ior )
    r@ file-pool free-pool ( ior )
    check-throw-ior ( )
  then ( )
  r> ( fil )
;

\ Close file
\ May throw x-fr-* exception.
\ ( fil -- )
: f_close
  dup fs_f_close ( fil ior )
  check-throw-ior ( fil )
  file-pool free-pool ( )
;

\ Internal variable used to hold a numbytes value.
0 variable (fs_nbytes)

\ Read n bytes from file into buffer
\ May throw x-fr-* exception.
\ ( fil buf numbytes -- numbytes )
: f_read
  (fs_nbytes)     \ ( fp buf btr *br )
  fs_f_read       \ ( ior )
  check-throw-ior \ ( )
  (fs_nbytes) @   \ ( nbytes )
;

\ Write n bytes from buffer into file.
\ May throw x-fr-* exception.
\ ( fil buf numbytes -- numbytes )
: f_write
  (fs_nbytes)        \ ( fp buf btw *bw )
  fs_f_write         \ ( ior )
  check-throw-ior    \ ( )
  (fs_nbytes) @      \ ( nbytes )
;

\ Moves the file read/write pointer of an open file. Can also be used to expand
\ the file size (cluster pre-allocation).
\ May throw x-fr-* exception.
\ ( fil offset -- )
: f_lseek
  fs_f_lseek
  check-throw-ior
;

\ Truncate file size to current read/write pointer.
\ May throw x-fr-* exception.
\ ( fil -- )
: f_truncate
  fs_f_truncate
  check-throw-ior
;

\ Flush cached data
\ May throw x-fr-* exception.
\ ( fil -- )
: f_sync
  fs_f_sync
  check-throw-ior
;

\ Read a string from the file
\ May throw x-fr-* exception.
\ ( fil buf buflen -- adr len )
: f_gets
  rot >r ( buf buflen)
  r@ -rot ( fil buf buflen )
  fs_f_gets ( adr )
  r> fs_f_error 0= averts x-fr-int-err ( adr )
  dup if ( adr )
    dup s0len ( adr len )
  else
    0 ( 0 0 )
  then
;

\ Write a character to the file
\ May throw x-fr-* exception.
\ ( fil c -- )
: f_putc
  fs_f_putc ( bw )
  1 = averts x-fr-int-err ( )
;

\ Get current read/write pointer
\ ( fil -- rwptr )
: f_tell fs_f_tell ;

\ Test for end of file.
\ ( fil -- flag )
: f_eof fs_f_eof ;

\ Get file size
\ ( fil -- size )
: f_size fs_f_size ;

\ Test for an error
\ ( fil -- flag )
: f_error fs_f_error ;

\
\ Directory Access:
\

\ Open the directory specified in the input string.
\ May throw x-fr-* and x-pool-* exceptions.
\ ( addr len -- dir )
: f_opendir
  path str>path ( )
  dir-pool allocate-pool ( dir )
  dup path ( dir dir path )
  fs_f_opendir ( dir ior )
  ?dup if ( dir ior )
    over ( dir ior dir )
    dir-pool free-pool ( dir ior )
    check-throw-ior ( dir )
  then ( dir )
;

\ Close directory
\ May throw x-fr-* exception.
\ ( dir -- )
: f_closedir
  dup fs_f_closedir ( dir ior )
  check-throw-ior ( dir )
  dir-pool free-pool ( )
;

\ Read directory entry into the filinfo object.
\ May throw x-fr-* exception.
\ ( dir -- )
: f_readdir
  filinfo
  fs_f_readdir
  check-throw-ior
;

\ Open the directory specified in addr/len1 input string and read first
\ item matching pattern specified in addr/len2 input string.
\ Put result in filinfo object.
\ May throw x-fr-* exception.
\ ( addr1 len1 addr2 len2 -- dir )
: f_findfirst
  path2 str>path path str>path ( )
  dir-pool allocate-pool ( dir )
  dup filinfo path path2 ( dir dir nfo path pattern )
  fs_f_findfirst ( dir ior )
  ?dup if ( dir ior )
    over ( dir ior dir )
    dir-pool free-pool ( dir ior )
    check-throw-ior ( dir )
  then ( dir )
;

\ Find next entry matching pattern.
\ Put result in filinfo object.
\ May throw x-fr-* exception.
\ ( dir -- )
: f_findnext
  filinfo
  fs_f_findnext
  check-throw-ior
;

\
\ File and Directory Management:
\

\ Check existence of file or directory with given name. Put result in filinfo.
\ May throw x-fr-* exception.
\ ( addr len -- )
: f_stat
  path str>path ( )
  path filinfo ( path filinfo )
  fs_f_stat ( ior )
  check-throw-ior ( )
;

\ Remove file or subdirectory with given name.
\ May throw x-fr-* exception.
\ ( addr len -- )
: f_unlink
  path str>path ( )
  path ( path )
  fs_f_unlink ( ior )
  check-throw-ior ( )
;

\ Rename/move fil/dir in addr/len1 string addr/len2 string.
\ May throw x-fr-* exception.
\ ( addr1 len1 addr2 len2 -- )
: f_rename
  path2 str>path
  path str>path
  path path2 ( old_name new_name )
  fs_f_rename ( ior )
  check-throw-ior ( )
;

\ Change attribute of file or directory with given name.
\ Attribute values:
\ AM_RDO, AM_ARC, AM_SYS, AM_HID
\ May throw x-fr-* exception.
\ ( addr len attr mask -- )
: f_chmod
  >r >r ( addr len )
  path str>path ( )
  path r> r> ( path attr mask )
  fs_f_chmod ( ior )
  check-throw-ior ( )
;

\ Change timestamp (time and date) of file or subdirectory with given name to value specified in filinfo.
\ May throw x-fr-* exception.
\ ( addr len -- )
: f_utime
  path str>path ( )
  path filinfo ( path filinfo )
  fs_f_utime ( ior )
  check-throw-ior ( )
;

\ Make directory with given name.
\ May throw x-fr-* exception.
\ ( addr len -- )
: f_mkdir
  \ Make dir
  path str>path ( )
  path ( path )
  fs_f_mkdir ( ior )
  check-throw-ior ( )
;

\ Change current directory to directory with given name.
\ May throw x-fr-* exception.
\ ( addr len -- )
: f_chdir
  path str>path ( )
  path ( path )
  fs_f_chdir ( ior )
  check-throw-ior ( )
;

\ Return current working directory name.
\ The given string object remains valid until the next f_* operation.
\ May throw x-fr-* exception.
\ ( -- addr len )
: f_getcwd
  path 255 fs_f_getcwd ( ior )
  check-throw-ior ( )
  path path>str ( addr len )
;

\ Read one compare one buffer worth of data between to open files
\ ( fil0 fil1 buf0 buf1 -- noteqf bothzerof)
: (f_cmp_buf)
    2dup 2-rot ( buf0 buf1 fil0 fil1 buf0 buf1 )
    swap -rot ( buf0 buf1 fil0 buf0 fil1 buf1 )
    256 f_read ( buf0 buf1 fil0 buf0 len1 )
    -rot 256 f_read ( buf0 buf1 len1 len0 )
    2dup d0= >r ( buf0 buf1 len1 len0 R: bothzero )
    -rot ( buf0 len0 buf1 len1 R: bothzero )
    compare not ( noteq R: bothzero )
    r> ( noteq bothzero )
;

\ Compare two files, identified by filename. Pushes true if identical, false otherwise.
\ ( addr1 len1 addr2 len2 -- f )
: f_cmp
  FA_OPEN_EXISTING FA_READ or f_open ( addr1 len1 fil0 )
  -rot FA_OPEN_EXISTING FA_READ or f_open ( fil0 fil1 )
  temp-mark> ( fil0 fil1 mark0 )
  256 temp-allot temp-mark> ( fil0 fil1 mark0 mark1 )
  2>r ( fil0 fil1 R: mark0 mark1 )
  0 0
  begin ( fil0 fil1 f f R: mark0 mark1 )
    2drop ( fil0 fil1 R: mark0 mark1 )
    2dup 2r@ ( fil0 fil1 fil0 fil1 mark0 mark1 R: mark0 mark1 )
    (f_cmp_buf) ( fil0 fil1 noteq bothzero R: mark0 mark1 )
    2dup or ( fil0 fil1 noteq bothzero f R: mark0 mark1 )
  until ( fil0 fil1 noteq bothzero R: mark0 mark1 )
  rdrop ( fil0 fil1 noteq bothzero R: mark0 )
  r> >temp-mark ( fil0 fil1 noteq bothzero )
  swap not and ( fil0 fil1 f )
  -rot ( f fil0 fil1 )
  f_close
  f_close ( f )
;

\
\ Volume Management and System Configuration:
\

\ Get total and free bytes on volume.
\ May throw x-fr-* exception.
\ ( -- tot free )
: f_getfree
  s0" "
  fs_f_getfree ( tot free ior )
  check-throw-ior
;

)fs_fs";
