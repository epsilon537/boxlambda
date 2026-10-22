\ BoxLambda Forth
\ printf and sprintf adapted from repo:
\ https://github.com/jkotlinski/forth-strfmt

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
  over c@ ( src srcend c )
  '-' = dup if ( src srcend f )
    rot 1+ -rot ( src+1 srcend f )
  then
  left-justify c! ( src+1 srcend )
  over c@ ( src+1 srcend c )
  '0' = if ( src+1 srcend )
    swap 1+ swap ( src+2 srcend )
    '0' ( src+2 srcend '0' )
  else
    bl ( src+1 srcend bl )
  then
  pad-char c! ( src+1 srcend )
  base @ >r decimal ( src+1 srcend )
  over - ( src+1 len )
  0 0 2swap ( 0 0 src+1 len )
  >number ( numlo numhi addr len )
  2swap drop ( addr len numlo )
  min-field-width ! ( addr len )
  span ( addr addrend )
  r> base ! ( addr addrend )
;

: parse-cmdspec ( dst src srcend -- dst src srcend )
  swap 1+ swap ( dst src+1 srcend )
  parse-min-field-width ( dst src+1 srcend )
  base @ 2>r ( dst src+1 R: srcend base )
  dup >r ( dst src+1 R: srcend base src+1 )
  c@ case ( dst )
    [char] % of s" %" add-field endof ( dst )
    [char] c of swap charbuf c! charbuf 1 add-field endof
    [char] n of decimal swap dup s>d dabs <# #s rot sign #> add-field endof
    [char] u of decimal swap 0 <# #s #> add-field endof
    [char] x of hex swap 0 <# #s #> add-field endof
    [char] s of -rot add-field endof
    [char] d of decimal r> 1+ dup >r
      c@ case
        [char] n of -rot tuck dabs <# #s rot sign #> add-field endof
        [char] u of -rot <# #s #> add-field endof
      endcase
      endof
  endcase
  r> 2r> ( dst src+1 srcend base )
  base ! ( dst src+1 srcend )
;

\ Prints any into buffer addr2 using the format string at addr1 u.
\ addr2 u3 is the resulting string.
: sprintf ( any addr1 u1 addr2 -- addr2 u3 )
  dup >r ( any addr1 u1 addr2 R: addr2 )
  -rot ( any addr2 addr1 u1 R: addr2 )
  span ( any addr2 addr1 addr1end R: addr2 )
  begin
    2dup < while ( any addr2 addr1 addr1end R: addr2 )
      over c@ '%' = if ( any addr2 addr1 addr1end R: addr2 )
        parse-cmdspec
      else
        -rot ( any addr1end addr2 addr1 R: addr2 )
        2dup c@ swap c! ( any addr1end addr2 addr1 R: addr2 )
        -rot 1+ -rot ( any addr2+1 addr1 addr1end R: addr2 )
      then ( any addr2+1 addr1 addr1end R: addr2 )
      swap 1+ swap ( any addr2+1 addr1+1 addr1end R: addr2 )
  repeat ( addr2end addr1end addr1end R: addr2 )
  2drop ( addr2end R: addr2 )
  r> ( addr2end addr2 )
  tuck - ( addr2 u2 )
;

\ Prints any using the format string at c-addr u.
: printf ( any c-addr u -- ) 256 [: sprintf type ;] with-temp-allot ;

