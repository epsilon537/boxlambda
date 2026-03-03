const char escstr_fs[] =  R"escstr_fs(
\ Port of escsapedstrings.txt by Mattias Koch, extended to supported interpreter mode.

0 variable tmp-buf
0 variable tmp-size

: char-to-tmp ( c -- )
  tmp-size @ 256 u<
  if
    tmp-buf @ tmp-size @ + c!
    1 tmp-size +!
  else
    drop
  then
;

: string-empty ( addr len -- addr len ? ) dup 0= ;
: first-char   ( addr len -- addr len c ) over c@ ;
: remove-first ( addr len -- addr' len' ) 1- swap 1+ swap ;

: cut-first-char   ( addr len -- addr' len' c )
  string-empty if bl else first-char >r remove-first r> then
;

: escape-string ( addr len -- addr' len' )
  0 tmp-size !

  begin
    string-empty not
  while
    cut-first-char

    dup [char] \ =
    if
      drop
      cut-first-char

      case
        [char] t of      $09 endof \ Tab
        [char] n of      $0A endof \ LF
        [char] r of      $0D endof \ CR
        [char] e of      $1B endof \ ESC
        [char] \ of      $5C endof \ \
        [char] ' of [char] " endof \ "

        dup \ Keep character unchanged if it isn't recognized
      endcase
    then

    char-to-tmp
  repeat
  2drop

  tmp-buf @ tmp-size @
;

: esc-s" ( parses up to "  -- addr len )
  256 [:
    tmp-buf !
    state @ if
      [char] " parse
      escape-string
      postpone (s")
      string,
    else
      [char] " parse ( addr len )
      escape-string ( addr' len' )
      tuck ( len addr len )
      store-str-heap ( len addr ) \ Allocate heap memory and store string in heap
      swap 2dup set-str-pool-entry ( addr len ) \ Store string addr/len in string-pool
    then
  ;] with-temp-allot
[immediate] ;

)escstr_fs";
