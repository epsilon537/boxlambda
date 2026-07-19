\ The definitions below come from the Mecrisp Quintus Forth distribution,
\ the foundation of BoxLambda's Forth.
\
\ -----------------------------------------------------------------------------
\   A few tools for dictionary wizardy
\ -----------------------------------------------------------------------------

-1 constant FLAG-INVISIBLE
$10 constant FLAG-IMMEDIATE
$80 constant FLAG-RAMALLOT
$100 constant FLAG-BUFFER

: immediate-constant create , FLAG-IMMEDIATE setflags does> @ ;

: executablelocation? ( addr -- ? )
  dup  addrinimem?              \ In imem
  over ramvar-here u< and     \ and below the variables and buffers
  swap addrinemem? or           \ or in emem ?
;

: link>link ( addr -- addr* ) 0 cells + ; 
: link>flags ( addr -- addr* ) 1 cells + ;
: link>name  ( addr -- addr* ) 2 cells + ;
: link>code  ( addr -- addr* ) 2 cells + skipstring ;

\ pre-wordlist code>link.
: code>link  ( entrypoint -- addr | 0 ) \ Try to find this code start address in dictionary
  >r
  dictionarystart
  begin
    dup link>code r@ = if 
      rdrop exit 
    then
    dictionarynext
  until
  drop rdrop 0
;

