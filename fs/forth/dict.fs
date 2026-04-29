\ The definitions below come from the Mecrisp Quintus Forth distribution,
\ the foundation of BoxLambda's Forth.
\
\ -----------------------------------------------------------------------------
\   A few tools for dictionary wizardy
\ -----------------------------------------------------------------------------

: executablelocation? ( addr -- ? )
  dup  addrinimem?              \ In imem
  over ramvar-here u< and     \ and below the variables and buffers
  swap addrinemem? or           \ or in emem ?
;

: link>flags ( addr -- addr* ) 1 cells + ;
: link>name  ( addr -- addr* ) 2 cells + ;
: link>code  ( addr -- addr* ) 2 cells + skipstring ;

0 variable searching-for
0 variable closest-found

: code>link  ( entrypoint -- addr | 0 ) \ Try to find this code start address in dictionary

    searching-for !
  0 closest-found !

  dictionarystart
  begin
    dup link>code searching-for @ = if dup closest-found ! then
    dictionarynext
  until
  drop

  closest-found @
;

: inside-code>link ( addr-inside -- addr | 0 ) \ Try to find this address inside of a definition

  dup executablelocation? not if drop 0 exit then  \ Do not try to find locations which are not executable

    searching-for !
  0 closest-found !

  dictionarystart
  begin

    dup link>code searching-for @ u<=
    if \ Is the address of this entry BEFORE the address which is to be found ?
      \ Distance to current   Latest best distance
      searching-for @ over -  searching-for @ closest-found @ -  <
      if dup closest-found ! then \ Is the current entry closer to the address which is to be found ?
    then

    dictionarynext
  until
  drop

  \ Do not cross RAM/IMEM borders:

  searching-for @ addrinimem?
  closest-found @ addrinimem? xor if 0 else closest-found @ then
;

: traceinside. ( addr -- )
  inside-code>link if
  ." ( "                 closest-found @ link>code   hex.
  ." + " searching-for @ closest-found @ link>code - hex.
  ." ) "
  closest-found @ link>name ctype
  then
;

: variable>link  ( location -- addr | 0 ) \ Try to find this variable or buffer in dictionary

    searching-for !
  0 closest-found !

  dictionarystart
  begin

    dup link>flags @ \ Fetch Flags of current definition

    $7FFFFFF0 and \ Snip off visibility bit and alloted size field
    dup $140 = swap $80 = or \ "Buffer" or "Ramallot"
    if
      dup link>code execute searching-for @ = if dup closest-found ! then
    then

    dictionarynext
  until
  drop

  closest-found @
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


