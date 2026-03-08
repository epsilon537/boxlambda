\ >number
: >digit ( char -- d true | 0 ) \ "to-digit"
  \ convert char to a digit according to base followed by true, or false if out of range
  dup [ char 9 1+ literal, ] <
  if [char] 0 - \ convert '0'-'9'
    dup 0< if drop 0 exit then \ reject < '0'
  else
    dup [char] a < if bl + then \ convert to lowercase, exploiting ascii
    [char] a -
    dup 0< if drop 0 exit then \ reject non-letter < 'a'
    #10 + \ convert 'a'-'z'
  then
  dup base @ < dup 0= if nip then ( d true | false ) \ reject beyond base
;

: >number ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 ) \ "to-number"
  2swap 2>r
  begin ( c-addr u ) ( r: ud.accum )
    dup while \ character left to inspect
      over c@ >digit
    while \ digit parsed within base
      2r> base @ s>d ud* ( c-addr u d.digit ud.accum ) \ scale accum by base
      rot m+ 2>r \ add current digit to accum
      1 /string ( c-addr1+1 u1-1 )
  repeat then
  2r> 2swap ( ud2 c-addr/2 u2 )
;


