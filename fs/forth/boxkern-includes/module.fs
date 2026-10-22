\ BoxLambda Forth
\
\ Module - Adds support for creating and managing modules (namespaces).
\ Builds on the wordlist.fs module.

max-order stack-create (wordlist-current-stack)
max-order stack-create (wordlist-search-order-stack) \ Used to temporarily save the search-order.

0 variable (module-prev-find-hook)

\ Get the xt of the following Word, to be found in given namespace. E.g. mymod ::' foo.
( wid "name" -- xt )
: ::'
  [: >r get-order r> swap 1+ set-order ;] execute
  '
  [: get-order nip 1- set-order ;] execute
  [immediate]
;

\ Compile into the definition, the xt of the following Word, to be found in given namespace E.g. mymod ::['] foo.
( wid "name" -- xt )
: ::[']
  [: >r get-order r> swap 1+ set-order ;] execute
  ' literal,
  [: get-order nip 1- set-order ;] execute
  [immediate] [compileonly]
;

\ Save current search-order on (wordlist-search-order-stack).
( -- )
: (save-order)
  get-order ( x1..xm m )
  0 do ( x1..xm )
    (wordlist-search-order-stack) stack-push
  loop
;

\ Restore previously saved search-order.
( -- )
: (restore-order)
  (wordlist-search-order-stack) stack-depth >r ( R: m )
  r@ 0 do (wordlist-search-order-stack) stack-pop ( x1 R: m )
  loop ( x1..xm-1 R: m )
  r>
  set-order
;

\ A temporary find hook, used to restore search-order after a <wid> :: <Word> search.
( addr len -- code-address flags )
: (find-restore-search-order)
  (module-prev-find-hook) @ execute \ Execute the find...
  (restore-order) \ ...then restore the search-order...
  (module-prev-find-hook) @ hook-find ! \ ...and remove the temporary find hook.
;

\ Just for the next Word search, replace search-order with the given wordlist.
\ Afterwards, restore the search order.
\ Usage: <wid> :: foo finds in wordlist <wid> the Word foo and executes/compiles it.
( wid -- )
: ::
  (save-order)
  1 set-order
  \ Install (find-drop-module) as a temporary find hook. The previous
  \ find hook will be restored by (find-drop-module) itself.
  hook-find @ (module-prev-find-hook) !
  ['] (find-restore-search-order) hook-find ! [immediate]
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
  \ Create a new wordlist wid and make a constant with it, using "name".
  wordlist dup immediate-constant ( wid )
  \ Take the name of the constant we just created and set it as wordlist name.
  (latest) @ link>name over wordlist-name! ( wid )
  continue-module
;

\ Revert search-order and current to the state before begin-module.
( -- )
: end-module
  get-current >r ( R: wid )
  (wordlist-current-stack) stack-pop set-current ( R: wid ) \ Restore previous current.
  get-order ( x1..xm m R: wid )
  \ The current module's wid is not necessarily at the top of the stack (there might have
  \ been additional imports). Pop wids off the stack until we get to the current module's,
  \ then we set what remains as the search-order.
  begin
    dup while ( x1..xm m R: wid )
      1- ( x1..xm m-1 R: wid )
      swap r@ = if ( x1..xm-1 m-1 R: wid )
        set-order
        rdrop exit
      then
  repeat
  0 ?assert
;

\ run-time portion of import
( wid -- )
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

\ Copy search-order to (wordlist-search-order-stack) with top-most
\ wid match filtered out.
( wid -- )
: (search-order-filter>search-order-stack)
  >r 
  get-order ( x1..xm m R: wid )
  begin
    dup while ( x1..xm m R: wid )
      1- swap ( x1..xm-1 m-1 xm R: wid )
      dup r@ <> if ( x1..xm-1 m-1 xm R: wid )
        (wordlist-search-order-stack) stack-push ( x1..xm-1 m-1 R: wid )
      else
        \ found the wid. Remove it from the search-order and replace the
        \ return stack item with 0, so further matches are not removed.
        drop rdrop
        0 >r
      then
  repeat ( 0 R: wid|0 )
  drop rdrop ( )
;

\ Install the (wordlist-search-order-stack) as search-order.
: (search-order-stack>search-order)
  (wordlist-search-order-stack) stack-depth >r ( R: m )
  r@ 0 do (wordlist-search-order-stack) stack-pop ( x1 R: m )
  loop ( x1..xm-1 R: m )
  r>
  set-order ( )
;

\ run-time portion of unimport
( wid -- )
: (unimport)
  (search-order-filter>search-order-stack)
  (search-order-stack>search-order)
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

