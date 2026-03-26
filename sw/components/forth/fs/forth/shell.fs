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

\ ls with *- or ?-containing "dir/file" pattern string: "ls ./*.fs" , "ls ../*"
\ * and ? are wildcards in the pattern string.
\ ( "pattern" -- )
: ls
  cr
  token ( pata patl )
  [: ( patha pathl )
      basename ( basea basel )
      filinfo.fattrib fattrib>str
      filinfo.getftime time>str
      filinfo.getfdate date>str
      filinfo.fsize
      s" %08n %s %s %s %s" printf cr ( dir )
  ;] ( pata patl xt )
  glob-each
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
;

\ pwd
\ ( -- )
: pwd
  f_getcwd type cr
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

0 0 2variable chmodvalmask

\ chmod <fname> +/-<attrib> . e.g. chmod dir/file.* -rdo
\ ( "pattern" "+/-attrib" -- )
: chmod
  cr
  token ( pata patl )
  token ( pata patl attra attrl )
  2dup str>attrib or 0= if
    ." Unknown attribute: " type cr
    2drop
  else
    ." Applying " 2dup type ."  to: " cr
    str>attrib chmodvalmask 2! ( pata patl )

    [: ( srcfa srcfl )
      2dup type cr
      chmodvalmask 2@
      f_chmod
    ;] ( pata patl xt )
    glob-each
  then
;

0 0 2variable dstdir

\ ( src-addr src-len dst-addr dst-len -- )
: _cp-file-to-file
  \ Open input file first so that if there's an exception
  \ we don't end up with an empty output file
  2swap FA_OPEN_EXISTING FA_READ or f_open ( dsta dstl ifil )
  -rot FA_CREATE_ALWAYS FA_WRITE or f_open ( ifil ofil )
  swap ( ofil ifil )
  256 [: ( ofil infil buf )
    >r ( ofil infil )
    begin ( ofil infil )
      2dup ( ofil infil ofil infil )
      r@ 256 ( ofil infil ofil infil buf btr )
      f_read ( ofil infil ofil br )
      dup while ( ofil infil ofil br )
        r@ swap ( ofil infil ofil buf btw )
        f_write ( ofil infil bw )
        drop ( ofil infil )
    repeat ( ofil infil )
    rdrop ( ofil infil ofil br )
    2drop ( ofil infil )
  ;] with-temp-allot
  f_close f_close ( )
;

\ ( src-addr src-len dst-addr dst-len -- )
: _mv-file-to-file
  2over ." Moving " 2dup type
  2swap ."  to " 2dup type cr
  \ f_rename doesn't work for directory iteration,
  \ using copy followed by remove instead.
  _cp-file-to-file
  f_unlink
;

\ ( pattern-addr pattern-len dst-dir dst-len -- )
: _mv-pattern-to-dir
  dstdir 2! ( pata patl )

  [: ( srcpa srcpl )
    2dup basename ( srcpa srcpl dstfa dstfl )
    dstdir 2@ ( srcpa srcpl dstfa dstfl dstda dstdl )
    2swap ( srcpa srcpl dstda dstdl dstfa dstfl )
    128 [:
      pathname ( srcpa srcpl dstpa dstpl )
      _mv-file-to-file ( )
    ;] with-temp-allot
  ;] ( pata patl xt )
  glob-each
;

\ mv source-file dest-file
\ mv source-file dest-dir
\ mv source-pattern dest-dir
\ * and ? are wildcards in the pattern string.
\ ( "from" "to" -- )
: mv
  cr
  token token ( src-addr src-len dst-addr dst-len )
  [: 2dup f_stat ;] try 0= ( src-addr src-len dst-addr dst-len f )
  filinfo.fattrib AM_DIR and 0> and if
    _mv-pattern-to-dir
  else
    _mv-file-to-file
  then
;

\ ( src-addr src-len dst-addr dst-len -- )
: _cp-file-to-file
  2swap ." Copying " 2dup type
  2swap ."  to " 2dup type cr
  _cp-file-to-file
;

\ ( pattern-addr pattern-len dst-dir dst-len -- )
: _cp-pattern-to-dir
  dstdir 2! ( pata patl )

  [: ( srcpa srcpl )
    2dup basename ( srcpa srcpl dstfa dstfl )
    dstdir 2@ ( srcpa srcpl dstfa dstfl dstda dstdl )
    2swap ( srcpa srcpl dstda dstdl dstfa dstfl )
    128 [:
      pathname ( srcpa srcpl dstpa dstpl )
      _cp-file-to-file ( )
    ;] with-temp-allot
  ;] ( pata patl xt )
  glob-each
;

\ cp source-file dest-file
\ cp source-file dest-dir
\ cp source-pattern dest-dir
\ * and ? are wildcards in the pattern string.
\ ( "from" "to" -- )
: cp
  cr
  token token ( src-addr src-len dst-addr dst-len )
  [: 2dup f_stat ;] try 0= ( src-addr src-len dst-addr dst-len f )
  filinfo.fattrib AM_DIR and 0> and if
    _cp-pattern-to-dir
  else
    _cp-file-to-file
  then
;

\ cat <filename>
\ ( "filename" -- )
: cat
  cr
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  256 [: ( fil buf )
    begin ( fil buf )
      over f_eof not while ( fil buf )
        2dup 256 f_gets ( fil buf addr len )
        type ( fil buf )
    repeat
    drop ( buf )
    f_close
  ;] with-temp-allot
;

\ rm <file or dir pattern string>, e.g. rm dir/*.txt
\ * and ? are wildcards in the pattern string.
\ ( "pattern" -- )
: rm
  cr
  token
  [: ( patha pathl )
    2dup s" Removing: %s" printf cr ( patha pathl )
    f_unlink ( dir )
  ;] ( pata patl xt )
  glob-each
;

\ FIXME: TBD
: touch
;

\ Change drive. Supported volumes are sd0: and ram:
\ ( "volume" -- )
: chdrv
  token f_chdrive
;
\ Unmount a volume. Supported volumes are sd0: and ram:
\ ( "volume" -- )
: umount
  token f_umount
;

\ Mount a volume. Supported volumes are sd0: and ram0:
\ ( "volume" -- )
: mount
  token f_mount
;

\ Get volume usage info. Supported volume names are sd0: and ram0:
\ ( "volume" -- )
: df
  cr
  token f_getfree s" Free: %n KB Total: %n KB" printf cr
;

: x-eof ." End Of File Exception" cr ;

\ Execute xt with all output suspended.
\ usage: [: <statements to be executed with output suspended :] >null
\ ( xt -- )
: >null
  emit>null
  execute
  emit>console
;

\ Redirect stdout to given file in overwrite mode. Don't emit to console.
\ usage: [: <statements to be executed with redirection ;] >file file.log
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
\ usage: [: <statements to be executed with redirection ;] tee>file file.log
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
\ usage: [: <statements to be executed with redirection ;] >>file file.log
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
\ usage: [: <statements to be executed with redirection ;] tee>>file file.log
\ ( xt "filename -- )
: &>>file
  token FA_OPEN_APPEND FA_WRITE or f_open ( xt fil )
  >r ( xt R: fil )
  r@ tee>file ( xt R: fil )
  execute ( R: fil )
  tee-end
  r> f_close
;

#128 constant MAX-LINE-LENGTH
#10 constant NEWLINE

: x-line-truncated MAX-LINE-LENGTH s" Line got truncated. Max. length = %n." printf ;

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

0 variable include-verbose
0 variable include-source-id

\ Load forth code from a file with the specified path.
\ include may be used recursively, i.e. the file being
\ included itself may contain one or more include calls.
\ May raise x-line-truncated
\ ( "path" -- )
: include
  include-source-id @ include-push
  token FA_OPEN_EXISTING FA_READ or f_open ( fil )
  dup include-source-id ! >r ( R: fil )
  begin ( n*x R: fil )
    r@ f_eof not while ( n*x R: fil )
      r@ tib MAX-LINE-LENGTH f_gets ( n*x addr len R: fil )
      \ if the line doesn't end with \n, it got truncated
      2dup + 1- c@ NEWLINE <> triggers x-line-truncated ( n*x addr len R: fil )
      include-verbose @ if ( n*x addr len R: fil )
        2dup type
      then
      \ strip trailing \n
      1- evaluate ( n*x R: fil mark )
  repeat
  r> f_close ( n*x )
  include-pop ( source-id )
  include-source-id !
;

\ An alternative query that also supports input
\ from file. The file id is indicated in variable
\ include-source-id. If set to 0, refill invokes
\ query.
\ May raise x-line-truncated and x-eof
\ ( -- )
: refill
  include-source-id @ ?dup if ( fil )
    0 >in !
    dup f_eof triggers x-eof ( fil )
    tib MAX-LINE-LENGTH f_gets ( adr len )
    \ if the line doesn't end with \n, it got truncated
    2dup + 1- c@ NEWLINE <> triggers x-line-truncated ( addr len )
    \ strip trailing \n
    1- setsource ( )
  else
    query
  then
;

