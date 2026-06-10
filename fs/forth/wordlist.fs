\ BoxLambda Forth
\
\ Wordlists - Adds support for creating multiple wordlists and
\ specifying across which wordlists, and in which order, word search (find)
\ should take place and specifying to which wordlist new words should be added.

\ The maximum number of wordlists in the search-order list.
16 constant max-order

$ffffffff constant erasedcell

max-order 1+ cells buffer: search-order

: x-order-overflow ( -- ) ." searc-order list overflow" cr ;

: x-empty-search-order ( -- ) ." search-order list empty" cr ;

\ Set the search order list and the length (n) of the search order list.
\ widn is the first wordlist to search, wid0 the last.
\ May throw x-order-overflow.
( wid0 ... widn n | -1 )
: set-order
  dup max-order > triggers x-order-overflow
  dup 0= triggers x-empty-search-order
  dup >r 0 ?do
    i cells search-order + ! 
  loop
  0 r> cells search-order + !  \ zero terminated order
;

\ Return the number of wordlists (wids) in the search order.
: (search-order-n) ( -- n )
  0 search-order 
  begin ( 0 iter ) 
    dup @ ( 0 iter wid ) 
    while ( 0 iter )
      swap 1+ swap ( 1 iter )
      cell+ ( 1 iter ) 
  repeat 
  drop ( n )
; 

\ Retrieve the search order list and the length (n) of that list.
\ widn is the first wordlist being search, wid0 is the last.
( -- wid0 ... widn n )
: get-order
  (search-order-n) dup 0= if exit then ( n )
  dup >r ( n R: n )
  cells search-order + ( iter R: n )
  begin 
    1 cells - ( wid0..widx iter' R: n ) 
    dup @ ( wid0..widx iter wid R: n )
    swap ( wid0..widy iter R: n )
    dup search-order = ( wid0..widy iter f R: n )
  until
  drop ( wid0..widn R: n )
  r> ( wid0..widn n )
;

\ Print the wordlist search order, first-to-last.
( -- )
: .order
  get-order 0 ?do . space loop
;

\ Create a new wordlist and return its wordlist-id (wid). 
( -- wid )
: wordlist 
  here ( wid ) 
  \ Put the (wordlist-end) sentinel word in it,
  \ so a wordlist always contains at least one word.
  ['] (end-sentinel) code>link ,
;

\ Get the current wordlist. New words are added to the current wordlist.
( -- wid )
: get-current
  (current) @
;

\ Set the current wordlist. New words are added to the current wordlist.
( wid -- )
: set-current
  (current) !
;

get-current constant forth

\ wordlistptr keeps track of the current wordlist in the search-order being searched.
search-order variable wordlistptr

\ Fetches the next entry in the wordlist chain. Returns true if end of the wordlist is reached.
( link-addr - addr flag )
: wid-next
  \ Follow the link to the next word.
  link>link @ ( link-addr ) 
  \ check if it's pointing to a valid word.
  dup erasedcell = ( link-addr true/false )
;

\ Current entry point for the given wordlist.
: wid-start ( wid -- lfa )
  @ ( lfa )
;

\ List all the words in a give wordlist.
: wid-list ( wid -- )
  cr
  wid-start
  begin
    dup 8 + ctype space
    wid-next
  until
  drop
;

\ Scans dictionary chain search-order aware and returns true if end is reached.
( link-addr -- addr flag)
: dictionarynext
  \ Follow the link to the next word.
  link>link @ ( link-addr ) 
  \ check if it's pointing to a valid word.
  dup erasedcell <> if ( link-addr )
  \ link is pointing to a valid word. Return false.
    false exit ( link-addr false )
  then
  drop \ End of current wordlist reached. Move on to next one in the search-order.
  cell wordlistptr +! ( )
  wordlistptr @ @ ( wid )
  dup if ( wid ) \ wid is valid. Recurse.
    recurse exit
  then ( 0 )
  true ( 0 true ) \ End of search order reached. Return true.
;

\ This is the wordlist search-order aware version of dictionarystart.
( -- link-addr )
: dictionarystart
  \ (Re)Set the wordlistptr to the start of the search-order.
  search-order dup wordlistptr ! ( search-order )
  \ Get the first wid in the search-order.
  @ dup ?assert ( wid )
  \ Return the first word in the wid wordlist.
  @ ( link-addr )
;

\ This is the wordlist search-order aware version of find.
\ It works exactly like the core find word, but it's using
\ the redefined search-order awre dictionarystart/next words.
( addr len -- code-address flags )
: (find-wordlist)
  dictionarystart ( addr len link )
  begin ( addr len link )
    dup link>flags @ Flag_invisible <> if ( addr len link )
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
  forth 1 set-order
  ['] (find-wordlist) hook-find !
;

wordlist-init

