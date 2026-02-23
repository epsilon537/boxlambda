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

\ cd <dirname>
: cd
  token f_chdir
  cr
;

\ pwd
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
: chmod
  token ( addr len )
  token str>attrib ( addr len val mask )
  f_chmod ( )
;

\ mv from to
: mv
  token token f_rename
;

\ cp from to
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

: df
  cr
  f_getfree s" Total: %n Free: %n" printf
  cr
;

\ Redirect input stream to given file.
\ Input will be read from file until EOF is reached.
\ ( "filename" -- )
: <file
  token FA_OPEN_EXISTING FA_READ or f_open
  [: key<console key-fil @ f_close ;] key<file
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

\ This word is invoked when an include file reaches
\ end-of-file. It switches the redirected input
\ to the previous include file descriptor on the stack,
\ or, when all include files have been processed, to
\ the console.
\ ( -- )
: include-eof
  \ Close the file on top of the stack, this is the
  \ file that has reached eof.
  include-pop f_close
  \ Pop the next file off the stack and check if it's
  \ an actual file.
  include-pop ?dup if
    \ It is a file. Put it back on the stack and
    \ redirect the input to this file.
    \ Keep the same eof handler.
    dup include-push
    eof-hook @ key<file
  else
    \ A 0 on the stack indicates we should switch
    \ input back to the console.
    key<console
  then
;


\ Load forth code from a file with the specified path.
\ include may be used recursively, i.e. the file being
\ included itself may contain one or more include calls.
\ ( "path" -- )
: include
  \ If this is the first (i.e. top-level) include, push
  \ a 0 on the include stack. The 0 indicates when to
  \ switch back the input to the console.
  include-sp @ 0= if
    0 include-push
  then
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  dup include-push ( fil )
  \ Redirect input to the opened file, invoke
  \ include-eof when the end of the file is reached.
  ['] include-eof key<file
;

)shell_fs";

