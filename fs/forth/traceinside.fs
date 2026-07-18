\ Traceinside implementation.

0 variable closest-found-wid

: inside-code>link ( addr-inside -- addr | 0 ) \ Try to find this address inside of a definition

  dup executablelocation? not if drop 0 exit then  \ Do not try to find locations which are not executable

  searching-for !
  0 closest-found !

  dictionarystart-all-wids ( addr wid )
  begin
    swap ( wid addr )
    dup link>code searching-for @ ( wid addr code searching-for )
    u<= if \ Is the address of this entry BEFORE the address which is to be found ?
      \ Distance to current   Latest best distance ( wid addr )
      searching-for @ over -  ( wid addr dist )
      searching-for @ closest-found @ - ( wid addr dist prevdist )  
      < if ( wid addr )
        dup closest-found ! ( wid addr )
        over closest-found-wid ! ( wid addr )
      then \ Is the current entry closer to the address which is to be found ?
    then ( wid addr )

    nip ( addr )
    dictionarynext-all-wids ( addr wid flag )
  until
  2drop

  \ Do not cross RAM/IMEM borders:

  searching-for @ addrinimem?
  closest-found @ addrinimem? xor if 0 else closest-found @ then
;

\ Find which word addr belongs to.
: traceinside. ( addr -- )
  inside-code>link if
  ." ( "                 closest-found @ link>code   hex.
  ." + " searching-for @ closest-found @ link>code - hex.
  ." ) "
  closest-found-wid @ wordlist-name@ ctype ."  :: "
  closest-found @ link>name ctype
  then
;


