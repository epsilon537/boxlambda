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
;

256 buffer: catbuf

\ cat <filename>
: cat
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  cr
  begin ( fil )
    dup catbuf 256 f_gets ( fil addr len )
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

: touch
;

: df
  cr
  f_getfree s" Total: %n Free: %n" printf
  cr
;

)shell_fs";

