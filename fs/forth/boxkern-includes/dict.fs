\ BoxLambda Forth
\ This is an continuation of pre-dict.fs with dependencies on wordlist.fs.

\ Traceinside implementation.

0 variable closest-found
0 variable searching-for

( link  -- f )
: (closer-found?)
  searching-for @ swap - ( dist )
  searching-for @ closest-found @ - ( dist prev-dist )
  < ( f )
;

( link -- )
: (check-update-closer-found)
  \ Is the address of this entry BEFORE the address which is to be found ?
  dup link>code searching-for @ u<= if
    \ Distance to current < Latest best distance? ( link )
    dup (closer-found?) if ( link )
      dup closest-found ! ( link )
    then
  then ( link )
  drop
;

 \ Try to find this address inside of a definition
: inside-code>link ( addr-inside -- link|0 )

  dup executablelocation? not if drop 0 exit then  \ Do not try to find locations which are not executable

  searching-for !
  0 closest-found !

  dictionarystart-all-wids ( link wid )
  begin
    drop dup (check-update-closer-found) ( link )
    dictionarynext-all-wids ( link wid|0 )
    dup 0=
  until
  2drop ( )

  \ Do not cross RAM/IMEM borders:
  searching-for @ addrinimem? closest-found @ addrinimem? xor ( f ) 
  if 0 else closest-found @ then
;

\ Find the wordlist a given link belongs to. 
: link>wid  ( link -- wid | 0 ) \ Try to find this code start address in dictionary
  searching-for !
  dictionarystart-all-wids
  begin ( dict-link dict-wid )
    over searching-for @ = if ( dict-link dict-wid )
      nip exit
    then 
    drop dictionarynext-all-wids ( dict-link dict-wid )
    dup 0=
  until
  2drop 0
;

\ wordlist aware code>link.
: code>link  ( entrypoint -- addr | 0 ) \ Try to find this code start address in dictionary
  searching-for !
  dictionarystart-all-wids drop ( link )
  begin
    dup link>code searching-for @ = if ( link )
      exit
    then ( link )
    dictionarynext-all-wids ( link wid|0 )
    0=
  until
  drop 0
;

: variable>link  ( location -- addr | 0 ) \ Try to find this variable or buffer in dictionary
  searching-for !
  dictionarystart-all-wids drop ( link )
  begin ( link )
    dup link>flags @ \ Fetch Flags of current definition ( link flags )
    $7FFFFFF0 and \ Snip off visibility bit and alloted size field ( link flags )
    dup FLAG-BUFFER = swap FLAG-RAMALLOT = or ( link f )
    if
      dup link>code execute searching-for @ = if ( link )
        exit
      then
    then
    dictionarynext-all-wids ( link wid|0 )
    0=
  until
  drop 0
;

: variable-name. ( addr -- ) \ Print the name of this variable or buffer, if possible
  variable>link ?dup if link>name ctype then
;

: forget ( -- ) \ Usage: forget name
  ' code>link
  dup @ (latest) !
  (dp) !
;

: del ( -- ) \ Remove the latest definition in RAM.
    (latest) @ (dp) !
    (latest) @ @ (latest) !
;

\ Find which word addr belongs to.
: traceinside. ( addr -- )
  inside-code>link ( link|0 ) 
  dup if ( link|0 )
    dup link>code ." ( " hex. ( link )
    dup link>code ." + " searching-for @ swap - hex. ( link )
    ." ) "
    dup link>wid wordlist-name@ ctype ."  :: " ( link )
    link>name ctype
  then
;

