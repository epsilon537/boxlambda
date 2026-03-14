\ A convenient memory dump helper

: dump16 ( addr -- ) \ Print 16 bytes memory
  base @ >r hex
  $F bic
  dup hex. ." :  "

  dup 16 + over do
    i c@ u.2 space \ Print data with 2 digits
    i $F and 7 = if 2 spaces then
  loop

  ."  | "

  dup 16 + swap do
        i c@ 32 u>= i c@ 127 u< and if i c@ emit else [char] . emit then
        i $F and 7 = if 2 spaces then
      loop

  ."  |" cr
  r> base !
;

: dump ( addr len -- ) \ Print a memory region
  cr
  over 15 and if 16 + then \ One more line if not aligned on 16
  begin
    swap ( len addr )
    dup dump16
    16 + ( len addr+16 )
    swap 16 - ( addr+16 len-16 )
    dup 0<
  until
  2drop
;

