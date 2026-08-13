\ BoxLambda Forth
\
\ Wordlists - Adds support for creating multiple wordlists and
\ specifying across which wordlists, and in which order, word search (find)
\ should take place and specifying to which wordlist new words should be added.

compileto-save
compiletoimem

16 constant max-order \ The maximum number of wordlists in the search-order list.
128 constant max-wids \ The maximum number of wordlists

begin-structure wordlist-struct
  field: .wordlist-start
  field: .wordlist-name
end-structure

\ The initial content of a new wordlist
' (end-sentinel) code>link constant end-sentinel-link

\ Keeps track of all wordlists created.
\ A wid (wordlist id) is the address of an element in this array.
create wordlist-tbl max-wids wordlist-struct * allot
here constant wordlist-end

\ Set up the forth wid as the first entry in the wid-list
wordlist-tbl constant forth
(latest) @ link>name forth .wordlist-name !
(current) @ forth .wordlist-start !

\ Points to the next free entry in the wordlist-tbl
0 variable wordlist-top

$ffffffff constant erasedcell

\ search-order is a 0 terminated array of wids.
max-order 1+ array search-order

: x-wid-overflow ( -- ) ." Max. number of wids exceeded" cr ;

: x-order-overflow ( -- ) ." search-order list overflow" cr ;

: x-empty-search-order ( -- ) ." search-order list empty" cr ;

\ Set the search order list and the length (n) of the search order list.
\ widn is the first wordlist to search, wid0 the last.
\ May throw x-order-overflow.
( wid0 ... widn n -- )
: set-order
  dup max-order > triggers x-order-overflow
  dup 0= triggers x-empty-search-order
  dup >r ( wid0..widn n R: n )
  0 ?do
    i search-order ! 
  loop ( R: n )
  0 r> search-order !  \ zero terminated order
;

\ Return the number of wordlists (wids) in the search order.
: (search-order-n) ( -- n )
  0 0 search-order max-order 1+ find-in ( addr-of-0 )
  dup ?assert
  0 search-order - cell/
;

\ Retrieve the search order list and the length (n) of that list.
\ widn is the first wordlist being search, wid0 is the last.
( -- wid0 ... widn n )
: get-order
  (search-order-n) dup ?assert
  >r ( R: n )
  0 r@ 1- do
   i search-order @ ( wid0..widx R: n )
   -1
  +loop
  r> ( wid0..widn n )
;

\ Create a new wordlist and return its wordlist-id (wid).
( -- wid )
: wordlist
  wordlist-top @ ( top )
  dup wordlist-struct + ( top top' )
  dup wordlist-end < averts x-wid-overflow ( top top' )
  wordlist-top ! ( top )
;

\ Set the given counted string as wordlist name
( c-addr wid -- )
: wordlist-name! .wordlist-name ! ;

\ Get the name of this wid as a counted string
( wid -- caddr )
: wordlist-name@ .wordlist-name @ ;

\ Print the wordlist info
( wid -- )
: .wordlist
    dup hex. space 
    wordlist-name@ ctype cr
;

\ Print the wordlist search order, first-to-last.
( -- )
: .order
  cr
  get-order 0 ?do 
    .wordlist
  loop
;

\ The current wordlist. New words are added to this list.
0 variable current-wid

\ Get the current wordlist. New words are added to the current wordlist.
( -- wid )
: get-current current-wid @ ;

\ Set the current wordlist. New words are added to the current wordlist.
( wid -- )
: set-current
  dup current-wid ! ( wid )
  .wordlist-start (current) !
;

\ Fetches the next entry in the wordlist chain. Returns true if end of the wordlist is reached.
( link-addr - addr flag )
: wordlist-next
  link>link @ \ Follow the link to the next word. ( link-addr ) 
  dup erasedcell = \ check if it's pointing to a valid word. ( link-addr true/false )
;

\ Current entry point for the given wordlist.
: wordlist-start ( wid -- lfa ) .wordlist-start @ ;

\ List all the words in a given wordlist.
: wordlist-list ( wid -- )
  cr
  wordlist-start
  begin
    dup link>name ctype space
    wordlist-next
  until
  drop
;

\ wordlistptr keeps track of the current wordlist being searched by
\ the dictionarystart/next operations below.
0 variable wordlistptr

\ Scans dictionary chain search-order aware and returns true if end is reached.
( link-addr -- addr flag)
: dictionarynext
  link>link @ \ Follow the link to the next word. ( link-addr ) 
  dup erasedcell <> if \ check if it's pointing to a valid word. ( link-addr )
    false exit \ link is pointing to a valid word. Return false. ( link-addr false )
  then
  drop \ End of current wordlist reached. Move on to next one in the search-order.
  cell wordlistptr +! ( )
  \ wordlistptr points to a search-order cell, which points to a wid.
  wordlistptr @ @ ( wid )
  dup if ( wid ) \ wid is valid. Recurse.
    .wordlist-start recurse exit
  then ( 0 )
  true ( 0 true ) \ End of search order reached. Return true.
;

\ This is the wordlist search-order aware version of dictionarystart.
( -- link-addr )
: dictionarystart
  \ (Re)Set the wordlistptr to the start of the search-order.
  0 search-order dup wordlistptr ! ( search-order )
  @ \ Get the first wid in the search-order. ( wid )
  .wordlist-start @ \ Return the first word in the wid wordlist. ( link-addr )
;

\ This version of dictionarystart scans across all wids.
\ Both the word address and the wid it belongs to are returned.
( -- link-addr wid )
: dictionarystart-all-wids
  \ (Re)Set the wordlistptr to the start of the wordlist-tbl
  wordlist-tbl dup wordlistptr ! ( wordlistptr )
  .wordlist-start @ \ Return the first word of the first wordlist. ( link-addr )
  wordlistptr @     \ And its wid ( link-addr wid )
;

\ Scans dictionary chain across all wids, returns a word-address and its wid
\ if a word is found. Returns 0 0 if end is reached.
( link-addr -- addr wid|0 )
: dictionarynext-all-wids
  link>link @ \ Follow the link to the next word. ( link-addr ) 
  dup erasedcell <> if \ check if it's pointing to a valid word. ( link-addr )
    \ link is pointing to a valid word, return link-addr and wid
    wordlistptr @ exit ( link-addr wid )
  then
  drop \ End of current wordlist reached. Move on to next one.
  wordlist-struct wordlistptr +! wordlistptr @ ( wordlistptr )
  dup wordlist-top @ <> if \ not at the end yet? ( wordlistptr )
    .wordlist-start recurse exit
  then
  drop 0 false \ End of search order reached.
;

\ This is the wordlist search-order aware version of find.
\ It works exactly like the core find word, but it's using
\ the redefined search-order aware dictionarystart/next words.
( addr len -- code-address flags )
: (find-wordlist)
  dictionarystart ( addr len link )
  begin ( addr len link )
    dup link>flags @ FLAG-INVISIBLE <> if ( addr len link )
      >r ( addr len R: link )
      r@ link>name count ( addr len link-addr link-len R: link )
      2over compare if ( addr len R: link ) \ Found:
        2drop ( R: link )
        r@ link>code r> link>flags @ ( code-address flags )
        exit
      then ( addr len R: link )
      r> ( addr len link )
    then ( addr len link )
    dictionarynext ( addr len link end-flag )
  until ( addr len link ) 
  2drop drop ( )
  0 0
;

: wordlist-init
  c" (noname)" ( init-name )
  \ The first entry is taken by the forth wordlist,
  \ so point top to the 2nd entry
  wordlist-tbl wordlist-struct + dup wordlist-top ! ( init-name top )
  \ Init 2nd entry to end with links to the end-sentinel, so
  \ all wordlists contains at least one entry.
  wordlist-end swap do ( init-name )
    dup i .wordlist-name !
    end-sentinel-link i .wordlist-start !
  wordlist-struct +loop
 
  drop ( )

  \ The initial search order and current
  forth 1 set-order
  forth set-current

  \ Activate worlist-enabled find.
  ['] (find-wordlist) hook-find !
;

wordlist-init

compileto-restore

