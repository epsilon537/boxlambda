\ BoxLambda Forth
\
\ Module - Adds support for creating and managing modules (namespaces).
\ Builds on wordlists.

max-order stack-create (wordlist-current-stack)
max-order stack-create (wordlist-search-order-stack)

0 variable (module-prev-find-hook)

\ A temporary find hook only used to find the word after a ::,
\ then switch back to the previous find implementation.
( addr len -- code-address flags )
: (find-drop-module)
  (module-prev-find-hook) @ execute
  get-order nip 1- set-order
  (module-prev-find-hook) @ hook-find !
;

\ Get the xt of the following word, to be found in given namespace. E.g. mymod ::' foo
( wid "name" -- xt )
: ::'
  [: >r get-order r> swap 1+ set-order ;] execute
  '
  [: get-order nip 1- set-order ;] execute
  [immediate]
;

\ Compile into the definition, the xt of the following word, to be found in given namespace E.g. mymod ::['] foo
( wid "name" -- xt )
: ::[']
  [: >r get-order r> swap 1+ set-order ;] execute
  ' literal,
  [: get-order nip 1- set-order ;] execute
  [immediate] [compileonly]
;

\ Set wid at the top of the search order for the next word search,
\ then remove wid from the top of the search order.
( wid -- )
: ::
  >r get-order r> swap 1+ set-order
  hook-find @ (module-prev-find-hook) !
  ['] (find-drop-module) hook-find ! [immediate]
;

\ Extend the given module/namespace. It works like begin-module
\ but takes an existing module wid as input parameter rather than
\ creating a new one.
( wid -- )
: continue-module
  get-current (wordlist-current-stack) stack-push ( wid )
  dup set-current ( wid )
  >r get-order r> swap 1+ set-order ( )
;

\ Create a new module/namespace with the given name.
\ This creates a new wordlist, makes it current and puts it on
\ top of the search order. Assigns the wordlist id (wid) to 
\ a constant with the passed in name. This constant becomes the
\ module identifier.
( "name" -- )
: begin-module
  wordlist dup immediate-constant ( wid )
  (latest) @ link>name over wordlist-name! ( wid )
  continue-module
;

\ Revert search-order and current to the state before begin-module.
( -- )
: end-module
  get-current >r ( R: wid )
  (wordlist-current-stack) stack-pop set-current ( R: wid )
  get-order ( x1..xm m R: wid )
  begin
    dup while ( x1..xm m R: wid )
      1- ( x1..xm m-1 R: wid )
      swap r@ = if ( x1..xm-1 m-1 R: wid )
        set-order
        rdrop
        exit
      then
  repeat
  0 ?assert
;

\ run-time portion of import
: (import)
  >r get-order r> swap 1+ set-order ( )
;

\ Add the given module to the top of the wordlist search order.
( module -- )
: import
  state @ if
    postpone literal
    postpone (import)
  else
    (import)
  then
  [immediate]
;

\ run-time portion of unimport
: (unimport)
  >r 
  get-order ( x1..xm m R: wid )
  begin
    dup while ( x1..xm m R: wid )
      1- swap ( x1..xm-1 m-1 xm R: wid )
      dup r@ <> if ( x1..xm-1 m-1 xm R: wid )
        (wordlist-search-order-stack) stack-push ( x1..xm-1 m-1 R: wid )
      else
        drop
      then
  repeat ( 0 R: wid )
  drop rdrop ( )
  (wordlist-search-order-stack) stack-depth >r ( R: m )
  r@ 0 do (wordlist-search-order-stack) stack-pop ( x1 R: m )
  loop ( x1..xm-1 R: m )
  r>
  set-order ( )
;

\ Remove the given module from the search-order. If the module
\ appears more than once in the search-order, only the top-most
\ entry is removed.
( module -- )
: unimport
  state @ if
    postpone literal
    postpone (unimport)
  else
    (unimport)
  then
  [immediate]
;

