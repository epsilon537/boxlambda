\ From repo: https://github.com/jkotlinski/forth-strfmt

0 variable min-field-width
create left-justify 1 allot
create pad-char 1 allot
create charbuf 1 allot

: pad-left ( dst c-addr u -- dst c-addr u )
  left-justify c@ if
    exit
  then
  dup
  min-field-width @ < if
    min-field-width @ over - >r rot dup r@ pad-char c@ fill r> + -rot
  then
;

: pad-right ( dst u -- dst )
  tuck + swap left-justify
  c@ if
    min-field-width @ swap - >r
    r@ 0> if
      dup r@ bl fill r@ +
    then
      rdrop
    else
      drop
  then
;

: add-field ( dst c-addr u -- dst ) pad-left >r over r@ move r> pad-right ;

: parse-min-field-width ( src srcend -- src srcend )
  over ( src srcend src )
  c@ ( src srcend c )
  [char] - ( src srcend c '-' )
  = ( src srcend f )
  dup if ( src srcend f )
    rot 1+ -rot ( src+1 srcend f )
  then
  left-justify c! ( src+1 srcend )
  over ( src+1 srcend src+1 )
  c@ [char] 0 ( src+1 srcend c '0')
  = ( src+1 srcend f )
  if ( src+1 srcend )
    swap 1+ swap ( src+2 srcend )
    '0' ( src+2 srcend '0' )
  else
    bl ( src+1 srcend bl )
  then
  pad-char c! ( src+1 srcend )
  base @ >r decimal ( src+1 srcend )
  over  ( src+1 srcend src+1)
  - ( src+1 len )
  0 ( src+1 len 0 )
  -rot ( 0 src+1 len )
  0 ( 0 src+1 len 0 )
  -rot ( 0 0 src+1 len )
  >number ( numlo numhi addr len )
  rot ( numlo addr len numhi )
  drop ( numlo addr len )
  rot ( addr len numlo )
  min-field-width ! ( addr len )
  over ( addr len addr )
  + ( addr addrend )
  r> ( addr addrend base )
  base ! ( addr addrend )
;

: parse-cmdspec ( dst src srcend -- dst src srcend )
  swap 1+ swap ( dst src+1 srcend )
  parse-min-field-width ( dst src+1 srcend )
  base @ >r ( dst src+1)
  >r ( dst src+1 )
  dup >r ( dst src+1 )
  c@ ( dst c )
  case ( dst )
    [char] % of s" %" add-field endof ( dst )
    [char] c of swap charbuf c! charbuf 1 add-field endof
    [char] n of decimal swap dup s>d dabs <# #s rot sign #> add-field endof
    [char] u of decimal swap 0 <# #s #> add-field endof
    [char] s of -rot add-field endof
    [char] d of decimal r> 1+ dup >r
      c@ case
        [char] n of -rot tuck dabs <# #s rot sign #> add-field endof
        [char] u of -rot <# #s #> add-field endof
      endcase
      endof
  endcase
  r> r> ( dst src+1 srcend )
  r> base ! ( dst src+1)
;

\ Prints n*x into buffer c-addr2 using the format string at c-addr1 u.
\ caddr2 u3 is the resulting string.
: sprintf ( n*x caddr1 u1 caddr2 -- caddr2 u3 )
  dup >r ( n*x caddr1 u1 caddr2 )
  -rot ( n*x caddr2 caddr1 u1 )
  over ( n*x caddr2 caddr1 u1 caddr1 )
  + ( n*x caddr2 caddr1 caddr1end )
  begin
    2dup < while ( n*x caddr2 caddr1 caddr1end )
      over c@ [char] % = if ( n*x caddr2 caddr1 caddr1end )
        parse-cmdspec
      else
        -rot ( n*x caddr1end caddr2 caddr1 )
        2dup ( n*x caddr1end caddr2 caddr1 caddr2 caddr1 )
        c@ ( n*x caddr1end caddr2 caddr1 caddr2 c )
        swap ( n*x caddr1end caddr2 caddr1 c caddr2 )
        c! ( n*x caddr1end caddr2 caddr1 )
        -rot ( n*x caddr1 caddr1end caddr2 )
        1+ ( n*x caddr1 caddr1end caddr2+1 )
        -rot ( n*x caddr2+1 caddr1 caddr1end )
      then ( n*x caddr2+1 caddr1 caddr1end )
      swap 1+ swap ( n*x caddr2+1 caddr1+1 caddr1end )
  repeat ( caddr2end caddr1end caddr1end )
  2drop ( caddr2end )
  r> ( caddr2end caddr2 )
  tuck ( caddr2 caddr2end caddr2 )
  - ( caddr2 u2 )
;

\ Prints n*x using the format string at c-addr u.
: printf ( n*x c-addr u -- ) 256 [: sprintf type ;] with-temp-allot ;

