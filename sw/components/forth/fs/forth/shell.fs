const char shell_fs[] =  R"shell_fs(
\ Some basic shell-like commands for interactive use

\ Convert filinfo.fattrib to a string.
\ ( attrs -- addr len )
: fattrib>str
  >r
  r@ AM_RDO and if
    s" RDO"
  else
    s" ---"
  then
  r@ AM_ARC and if
    s" ARC"
  else
    s" ---"
  then
  r@ AM_SYS and if
    s" SYS"
  else
    s" ---"
  then
  r@ AM_HID and if
    s" HID"
  else
    s" ---"
  then
  r@ AM_DIR and if
    s" DIR"
  else
    s" ---"
  then

  rdrop

  s" %s %s %s %s %s"
  s" xxx-xxx-xxx-xxx-xxx" drop ( a1 l1 a2 l2 a3 l3 a4 l4 a5 l5 pata patl buf )
  sprintf
;

\ ( hh mm ss - addr len )
: time>str
  -rot swap ( ss mm hh )
  s" %02n:%02n:%02n"
  s" 00:00:00" drop ( ss mm hh pata patl buf )
  sprintf
;

\ ( dd mm yyyy - addr len )
: date>str
  s" %04n/%02n/%02n"
  s" 0000/00/00" drop ( yyyy mm dd pata patl buf )
  sprintf
;

\ ls with *- or ?-containing pattern string: "ls", "ls *.fs" , "ls *.?xt"
\ ( "pattern string" -- )
: ls
  s" ." token
  dup 0= if
    2drop
    s" *"
  then ( addr1 len1 addr2 len2 )
  f_findfirst ( dir )
  cr
  begin
    filinfo.fname ( dir addr1 len1 )
    dup 0> while
      filinfo.fattrib fattrib>str ( dir addr1 len1 addr2 len2 )
      filinfo.getftime time>str ( dir addr1 len1 addr2 len2 addr3 len3 )
      filinfo.getfdate date>str ( dir addr1 len1 addr2 len2 addr3 len3 addr4 len4 )
      filinfo.fsize ( dir addr1 len1 addr2 len2 addr3 len3 addr3 len4 size )
      s" %08n %s %s %s %s" printf ( dir )
      cr ( dir )
      dup f_findnext  ( dir )
  repeat
  2drop
  f_closedir ( )
;

\ mkdir <dirname>
\ ( "dirname" -- )
: mkdir
  token f_mkdir
;

\ cd <dirname>
\ ( "dirname" -- )
: cd
  token f_chdir
  cr
;

\ pwd
\ ( -- )
: pwd
  cr
  f_getcwd type
  cr
;

\ ( addr len - val mask )
: str>attrib
  2dup s" -rdo" compare if
    0 AM_RDO
  else
    2dup s" +rdo" compare if
      AM_RDO AM_RDO
    else
      2dup s" -arc" compare if
        0 AM_ARC
      else
        2dup s" +arc" compare if
          AM_ARC AM_ARC
        else
          2dup s" -sys" compare if
            0 AM_SYS
          else
            2dup s" +sys" compare if
              AM_SYS AM_SYS
            else
              2dup s" -hid" compare if
                0 AM_HID
              else
                2dup s" +hid" compare if
                  AM_HID AM_HID
                else
                  0 0
                then ( addr len val mask )
              then
            then
          then
        then
      then
    then
  then

  2nip ( val mask )
;

\ chmod <fname> +/-<attrib> . e.g. chmod test.txt -rdo
\ ( "fname" "+/-attrib" -- )
: chmod
  token ( addr len )
  token str>attrib ( addr len val mask )
  f_chmod ( )
;

\ mv from to
\ ( "from" "to" -- )
: mv
  token token f_rename
;

\ cp from to
\ ( "from" "to" -- )
: cp
  token FA_OPEN_EXISTING FA_READ or f_open ( infil )
  token FA_CREATE_ALWAYS FA_WRITE or f_open ( infil ofil )
  swap 256 [: ( ofil infil buf )
    ." 1: " .s cr
    >r ( ofil infil )
    begin ( ofil infil )
      2dup ( ofil infil ofil infil )
      r@ 256 ( ofil infil ofil infil buf btr )
      ." 2:" .s cr
      f_read ( ofil infil ofil br )
      dup while ( ofil infil ofil br )
        r@ swap ( ofil infil ofil buf btw )
        ." 3:" .s cr
        f_write ( ofil infil bw )
        drop ( ofil infil )
    repeat ( ofil infil )
    rdrop ( ofil infil ofil br )
    2drop ( ofil infil )
  ;] with-temp-allot
  ." 4:" .s cr
  f_close f_close ( )
;

\ cat <filename>
\ ( "filename" -- )
: cat
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  cr
  begin ( fil )
    dup 256 [: 256 f_gets ;] with-temp-allot
    dup while ( fil addr len )
      type ( fil )
  repeat
  cr
  2drop
  f_close
;

\ rm <file or dir pattern string>, e.g. rm *.txt
\ ( "pattern" -- )
: rm
  s" ." token ( addr1 len1 addr2 len2 )
  f_findfirst ( dir )
  cr
  begin
    filinfo.fname ( dir addr1 len1 )
    dup 0> while
      2dup s" Removing: %s" printf ( dir addr1 len1 )
      f_unlink ( dir )
      cr ( dir )
      dup f_findnext  ( dir )
  repeat
  2drop
  f_closedir ( )
;

\ FIXME: TBD
: touch
;

\ ( -- )
: df
  cr
  f_getfree s" Total: %n Free: %n" printf
  cr
;

: x-eof ." End Of File Exception" cr ;

\ Execute the given xt, taking input from the given file.
\ If end-of-file is reached before the xt completes,
\ an x-eof exception is raised.
\ Example usage:
\ create sbf 80 allot
\ sbf dup 80 [: accept ;] <file name.txt esc-s" \nHello %s\n" printf
\ ( xt "filename" -- )
: <file
  token FA_OPEN_EXISTING FA_READ or f_open >r ( xt )
  r@ [: key<console key-fil @ f_close ['] x-eof ?raise ;] key<file ( xt )
  execute ( )
  key<console
  r> f_close
;

\ Execute xt with all output suspended.
\ usage: [: <statements to be executed with output suspended :] >null
\ ( xt -- )
: >null
  emit>null
  execute
  emit>console
;

\ Redirect stdout to given file in overwrite mode. Don't emit to console.
\ usage: [: <statements to be executed with redirection :] >file file.log
\ ( xt "filename" -- )
: >file
  token FA_CREATE_ALWAYS FA_WRITE or f_open ( xt fil )
  >r ( xt R: fil )
  r@ emit>file ( xt R: fil )
  execute ( R: fil )
  emit>console
  r> f_close
;

\ Redirect stdout to given file in overwrite mode and emit to console.
\ usage: [: <statements to be executed with redirection :] tee>file file.log
\ ( xt "filename -- )
: &>file
  token FA_CREATE_ALWAYS FA_WRITE or f_open ( xt fil )
  >r ( xt R: fil )
  r@ tee>file ( xt R: fil )
  execute ( R: fil )
  emit>console
  r> f_close
;

\ Redirect stdout to given file in append mode. Don't emit to console.
\ usage: [: <statements to be executed with redirection :] >>file file.log
\ ( xt "filename" -- )
: >>file
  token FA_OPEN_APPEND FA_WRITE or f_open ( xt fil )
  >r ( xt R: fil )
  r@ emit>file ( xt R: fil )
  execute ( R: fil )
  emit>console
  r> f_close
;

\ Redirect stdout to given file in append mode and emit to console.
\ usage: [: <statements to be executed with redirection :] tee>>file file.log
\ ( xt "filename -- )
: &>>file
  token FA_OPEN_APPEND FA_WRITE or f_open ( xt fil )
  >r ( xt R: fil )
  r@ tee>file ( xt R: fil )
  execute ( R: fil )
  tee-end
  r> f_close
;

\ A stack to keep track of recursive includes.
create include-stack MAX_NUM_OPEN_FILES cells allot
0 variable include-sp   \ stack pointer
0 variable saved-quit-hook

\ Push include file descriptor on the include-stack.
\ ( x -- )
: include-push
  include-sp @ ( x sp )
  dup MAX_NUM_OPEN_FILES < ?assert ( x sp )
  tuck cells include-stack + ! ( sp )
  1+ include-sp ! ;

\ Pop include file descriptor from the include-stack.
\ ( -- x )
: include-pop
  include-sp @ ( sp )
  dup 0> ?assert ( sp )
  1- dup include-sp ! ( sp-1 )
  cells include-stack + @ ( x )
;

0 variable include-source-id

\ Load forth code from a file with the specified path.
\ include may be used recursively, i.e. the file being
\ included itself may contain one or more include calls.
\ ( "path" -- )
: include
  include-source-id @ include-push
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  dup include-source-id ! ( fil )
  80 [: ( fil buf )
    begin ( fil buf )
      2dup 80 f_gets ( fil buf adr len )
      dup 0> while ( fil buf adr len )
        1- evaluate ( fil buf )
    repeat
  ;] with-temp-allot ( fil buf adr len )
  2drop drop ( fil )
  f_close ( )
  include-pop ( source-id )
  include-source-id !
;

\ An alternative query that also supports input
\ from file. The file id is indicated in variable
\ include-source-id. If set to 0, refill invokes
\ query.
\ ( -- )
: refill
  include-source-id @ ?dup if
    80 [: ( fil buf )
      80 f_gets ( adr len )
      setsource ;] with-temp-allot
  else
    query
  then
;

)shell_fs";

