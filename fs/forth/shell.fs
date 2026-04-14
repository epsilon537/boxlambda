\ BoxLambda Forth
\ Some basic shell-like commands for interactive use

\ Copy attribute string template into string object
\ to be selectively modified by _---_if_not_set below.
\ ( -- addr len )
: _attrstr
  s" RDO HID SYS XXX DIR ARC" drop
  s"                        " ( a0 a1 l1 )
  2dup 2>r ( a0 a1 l1 R: a1 l1 )
  move ( R: a1 l1)
  2r> ( a1 l1 )
;

\ If attribute at given bit position if not set,
\ replace attribute name in string object with ---
\ ( addr attrs bitnum -- )
: _---_if_not_set
  >r ( addr attrs R: bitnum )
  r@ bitval and 0= if ( addr R: bitnum )
    r@ 4 * + ( addr+offset R: bitnum )
    dup 3 '-' fill ( R: bitnum )
  then
  drop rdrop ( )
;

\ Convert filinfo.fattrib to a string.
\ ( attrs -- addr len )
: fattrib>str
  _attrstr >r swap ( addr attrs R: len )
  6 0 do ( addr attrs )
    2dup i _---_if_not_set ( addr attrs )
  loop
  drop r> ( addr len )
;

\ Convert filesystem time object to string
\ ( hh mm ss - addr len )
: time>str
  -rot swap ( ss mm hh )
  s" %02n:%02n:%02n"
  s" 00:00:00" drop ( ss mm hh pata patl buf )
  sprintf
;

\ Convert filesystem date object to string
\ ( dd mm yyyy - addr len )
: date>str
  s" %04n/%02n/%02n"
  s" 0000/00/00" drop ( yyyy mm dd pata patl buf )
  sprintf
;

\ ls with glob pattern string: "ls ./*.fs" , "ls ../*"
\ ( "glob-pattern" -- )
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

\ +/-rdo/hid/sys/xxx/dir/arc -> attrib value and mask
\ ( addr len -- val mask )
: str>attrib
  4 = if ( ain )
    1+
    s" rdohidsysxxxdirarc" drop ( ain aref )
    6 0 do
      2dup 3 tuck ( ain aref ain 3 aref 3 )
      compare if ( ain aref )
        drop ( ain )
        i bitval swap ( bit ain )
        1- c@ case ( bit )
          '-' of 0 swap endof ( 0 bit )
          '+' of dup endof ( bit bit )
          drop 0 0 ( 0 0 )
        endcase
        unloop exit
      then
      3 + ( ain aref+3 )
    loop ( ain aref )
    drop ( ain )
  then
  drop 0 0
;

0 0 2variable chmodvalmask

\ chmod <fname> +/-<attrib> . e.g. chmod dir/file.* -rdo
\ ( "glob-pattern" "+/-attrib" -- )
: chmod
  cr
  token token ( pata patl attra attrl )
  2dup str>attrib or
  0= if
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

\ ( src-addr src-len dst-addr dst-len -- )
: _cp-file-to-file
  \ Open input file first so that if there's an exception
  \ we don't end up with an empty output file
  2swap FA_OPEN_EXISTING FA_READ or f_open ( dsta dstl ifil )
  -rot FA_CREATE_ALWAYS FA_WRITE or f_open ( ifil ofil )
  256 [: ( ifil ofil buf )
    >r 2>r ( R: buf ifil ofil )
    begin
      1 rpick 2 rpick 256 f_read ( br )
      dup while ( br )
        0 rpick 2 rpick rot f_write ( bw )
        drop ( )
    repeat ( br )
    drop 2r> rdrop ( ifil ofil )
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

0 0 2variable dstdir

\ ( glob-addr glob-len dst-dir dst-len -- )
: _mv-glob-to-dir
  dstdir 2! ( globa globl )

  [: ( srcpa srcpl )
    2dup basename ( srcpa srcpl dstfa dstfl )
    dstdir 2@ ( srcpa srcpl dstfa dstfl dstda dstdl )
    2swap ( srcpa srcpl dstda dstdl dstfa dstfl )
    128 [:
      pathname ( srcpa srcpl dstpa dstpl )
      _mv-file-to-file ( )
    ;] with-temp-allot
  ;] ( globa globl xt )
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
    _mv-glob-to-dir
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

\ Load and evaluate forth code from a file with the given path.
\ include may be used recursively, i.e. the file being
\ included itself may contain one or more include calls.
\ May raise x-line-truncated
\ ( n*x "path" -- m*y )
: include
  include-source-id @ include-push
  token FA_OPEN_EXISTING FA_READ or f_open ( n*x fil )
  dup include-source-id ! >r ( n*x R: fil )
  begin ( n*x R: fil )
    r@ f_eof not while ( n*x R: fil )
      r@ tib MAX-LINE-LENGTH f_gets ( n*x addr len R: fil )
      \ if the line doesn't end with \n, it got truncated
      2dup + 1- c@ NEWLINE <> triggers x-line-truncated ( n*x addr len R: fil )
      include-verbose @ if 2dup type then ( n*x addr len R: fil )
      1- evaluate ( m*y R: fil mark ) \ 1- strips trailing \n
  repeat ( m*y R: fil )
  r> f_close ( m*y )
  include-pop include-source-id ! ( m*y )
;

\ An alternative query that also supports input
\ from file. The file id is indicated in variable
\ include-source-id. If set to 0, refill invokes
\ query.
\ May raise x-line-truncated and x-eof
\ ( -- )
: refill
  include-source-id @ ?dup if ( fil )
    0 >in ! ( fil )
    dup f_eof triggers x-eof ( fil )
    tib MAX-LINE-LENGTH f_gets ( adr len )
    \ if the line doesn't end with \n, it got truncated
    2dup + 1- c@ NEWLINE <> triggers x-line-truncated ( addr len )
    1- setsource ( ) \ 1- strips trailing \n
  else
    query
  then
;

