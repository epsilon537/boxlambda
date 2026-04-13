\ BoxLambda port of Zeptoforth's heap.fs module.
\
\ Original header:
\
\ Copyright (c) 2021-2023 Travis Bemann
\
\ Permission is hereby granted, free of charge, to any person obtaining a copy
\ of this software and associated documentation files (the "Software"), to deal
\ in the Software without restriction, including without limitation the rights
\ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
\ copies of the Software, and to permit persons to whom the Software is
\ furnished to do so, subject to the following conditions:
\
\ The above copyright notice and this permission notice shall be included in
\ all copies or substantial portions of the Software.
\
\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
\ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
\ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
\ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
\ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
\ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
\ SOFTWARE.

\ No blocks free exception
: x-allocate-failed ( -- ) ." allocate failed" cr ;

\ Internal error
: x-internal-error ( -- ) ." heap internal error" cr begin again ;

\ Memory is not allocated
: x-memory-not-allocated ( -- ) ." memory not allocated" cr ;

\ The heap structure
begin-structure heap-size
  \ The heap block count
  field: heap-block-count
  \ The heap block size
  field: heap-block-size
  \ Next free index
  field: heap-next-free
end-structure

\ The free block header structure
begin-structure free-header-size
  \ The free group size in blocks
  field: group-size
  \ The previous free group index
  field: group-prev-free
  \ The next free block index
  field: group-next-free
end-structure

\ Get the heap bitmap
: heap-bitmap ( heap -- bitmap )  heap-size + ;

\ Get a heap bitmap cell
: heap-bitmap-cell ( index heap -- addr )
   heap-bitmap swap 5 rshift cells +
;

\ Get the heap blocks
: heap-blocks ( heap -- blocks )
   dup heap-block-count @ swap heap-bitmap-cell
;

\ Get a block in a heap
: heap-block ( index heap -- block )
   dup heap-block-size @ rot * swap heap-blocks +
;

\ Get a block index in a heap by address
: block-addr>index ( addr heap -- index )
   tuck heap-blocks - swap heap-block-size @ /
;

\ Is a sequence of blocks fully within the heap
: in-heap? ( count index heap -- in-heap? )
  -rot + swap heap-block-count @ <=
;

\ Get the number of blocks for a size
: size>blocks ( size heap -- blocks )
  2dup heap-block-size @ mod 0 > if
  heap-block-size @ / 1+
  else
  heap-block-size @ /
  then
;

\ Get the value of the bitmap containing an index
: bitmap-index@ ( index heap -- bitmap )
  heap-bitmap swap 5 rshift 2 lshift + @
;

\ Get whether a block has been allocated
: block-allocated? ( index heap -- allocated? )
  swap tuck swap bitmap-index@ swap $1F and bitval and 0<>
;

\ Get the group size of a block
: group-size@ ( group heap -- blocks ) heap-block group-size @ ;

\ Set the group size of a block
: group-size! ( blocks group heap -- )
  2 pick 2 pick + over heap-block-count @ > triggers x-internal-error
  heap-block group-size !
;

\ Set the next free group in the heap
: heap-next-free! ( group heap -- )
  heap-next-free !
;

\ Get the previous free group index
: group-prev-free@ ( group heap -- index ) heap-block group-prev-free @ ;

\ Set the previous free group index
: group-prev-free! ( index group heap -- )
  heap-block group-prev-free !
;

\ Get the next free group index
: group-next-free@ ( group heap -- index ) heap-block group-next-free @ ;

\ Set the next free group index
: group-next-free! ( index group heap -- )
  heap-block group-next-free !
;

\ Get the end of a group
: group-end@ ( group heap -- index ) swap tuck swap group-size@ + ;

\ Get the highest zero bit in a cell lower than a given position
: test-high-zero ( u i -- i|-1 )
  over if
  over -1 <> if
  tuck 32 swap - lshift swap
  begin ?dup while
  over 31 rshift 0= if nip 1- exit then
  1- swap 1 lshift swap
  repeat
  drop -1
  else
  2drop -1
  then
  else
  nip 1-
  then
;

\ Get the highest nonzero bit in a cell lower than a given position
: test-high-nonzero ( u i -- i|-1 )
  over -1 <> if
  over if
  tuck 32 swap - lshift swap
  begin ?dup while
  over 31 rshift if nip 1- exit then
  1- swap 1 lshift swap
  repeat
  drop -1
  else
  2drop -1
  then
  else
  nip 1-
  then
;

\ Get the lowest zero bit in a cell higher than a given position
: test-low-zero ( u i -- i|-1 )
  over if
  over -1 <> if
  tuck rshift swap
  begin dup 32 < while
  over 1 and 0= if nip exit then
  1+ swap 1 rshift swap
  repeat
  2drop -1
  else
  2drop -1
  then
  else
  nip
  then
;

\ Get the end of the previous free block group
: find-prev-free-group-end ( index heap -- index|-1 )
  over 0 > if
  over $1F and if
  over >r swap $1F bic swap 2dup bitmap-index@ r> $1F and
  else
  swap 32 - swap 2dup bitmap-index@ 32
  then
  begin
  test-high-zero dup -1 <> if
  nip swap $1F bic + exit
  then
  drop over 0 > if
  swap 32 - swap 2dup bitmap-index@ 32 false
  else
  true
  then
  until
  2drop -1
  else
  2drop -1
  then
;

\ Get the start of a previous free block group
: find-free-group-start ( index heap -- index )
  over 0 > if
  over $1F and if
  over >r swap $1F bic swap 2dup bitmap-index@ r> $1F and
  else
  swap 32 - swap 2dup bitmap-index@ 32
  then
  begin
  test-high-nonzero dup -1 <> if
  nip swap $1F bic + 1+
  exit
  then
  drop over 0 > if
  swap 32 - swap 2dup bitmap-index@ 32 false
  else
  true
  then
  until
  2drop 0
  else
  2drop 0
  then
;

\ Get the start of the next free block group
: find-next-free-group-start ( index heap -- index|-1 )
  2dup heap-block-count @ < if
  over $1F bic over bitmap-index@
  begin
  2 pick $1F and test-low-zero dup -1 <> if
  nip swap $1F bic + exit
  then
  drop swap $1F bic 32 + swap 2dup heap-block-count @ < if
  2dup bitmap-index@ false
  else
  true
  then
  until
  2drop -1
  else
  2drop -1
  then
;

\ Link the next free block group
: link-group-next ( index next-index heap -- )
  >r
  dup -1 <> if
  2dup r@ group-prev-free! ( index next-index )
  then
  swap r> group-next-free! ( )
;

\ Link the previous free block group
: link-group-prev ( index prev-index heap -- )
  >r
  dup -1 <> if
  r@ find-free-group-start ( index prev-start-index )
  2dup r@ group-next-free! ( index prev-start-index )
  swap r> group-prev-free! ( )
  else
  drop dup r@ heap-next-free! -1 swap r> group-prev-free! ( )
  then
;

\ Link an adjacent next free block group
: link-group-next-adjacent ( index next-index heap -- )
  >r
  dup r@ group-next-free@ ( index next-index next-next-index )
  dup -1 <> if
  2 pick over r@ group-prev-free! ( index next-index next-next-index )
  then
  2 pick r@ group-next-free! ( index next-index )
  r@ group-size@ over r@ group-size@ + ( index size )
  swap r> group-size! ( )
;

\ Link an adjacent previous free block group
: link-group-prev-adjacent ( index prev-index heap -- )
  >r
  dup -1 <> if
  r@ find-free-group-start ( index prev-start-index )
  over r@ group-size@ ( index prev-start-index size )
  rot r@ group-next-free@ ( prev-start-index size next-index )
  -rot over r@ group-size@ ( next-index prev-start-index size prev-size )
  + over r@ group-size! ( next-index prev-start-index )
  over -1 <> if
  2dup swap r@ group-prev-free! ( next-index prev-start-index )
  then
  r> group-next-free! ( )
  else
  drop -1 swap r> group-prev-free! ( )
  then
;

\ Mark space as allocated
: mark-allocated ( count index heap -- )
  swap tuck swap heap-bitmap-cell >r
  begin over while
  32 over $1F and - 2 pick min
  $FFFFFFFF over 32 swap - rshift 2 pick $1F and lshift

  r@ bis! r> cell+ >r
  tuck + -rot - swap
  repeat
  rdrop 2drop
;

\ Mark space as freed
: mark-free ( count index heap -- )
  swap tuck swap heap-bitmap-cell >r
  begin over while
  32 over $1F and - 2 pick min
  $FFFFFFFF over 32 swap - rshift 2 pick $1F and lshift
  r@ bic! r> cell+ >r
  tuck + -rot - swap
  repeat
  rdrop 2drop
;

\ Link a freed block group
: link-group ( index heap -- )
  >r
  dup r@ find-prev-free-group-end ( index prev-end )
  2dup 1+ = over -1 <> and if
  over r@ group-end@ ( index prev-end end )
  r@ find-next-free-group-start ( index prev-end next-start )
  2 pick r@ group-end@ ( index prev-end next-start end )
  over = if ( index prev-end next-start )
  2 pick swap r@ link-group-next-adjacent ( index prev-end )
  else ( index prev-end next-start )
  2 pick swap r@ link-group-next ( index prev-end )
  then
  r> link-group-prev-adjacent ( )
  else
  over r@ group-end@ ( index prev-end end )
  r@ find-next-free-group-start ( index prev-end next-start )
  2 pick r@ group-end@ ( index prev-end next-start end )
  over = if ( index prev-end next-start )
  2 pick swap r@ link-group-next-adjacent ( index prev-end )
  else
  2 pick swap r@ link-group-next ( index prev-end )
  then
  r> link-group-prev ( )
  then
;

\ Check whether a group is expandable
: expandable-group? ( size index heap -- flag )
  >r dup r@ group-end@ ( size index end )
  dup r@ find-next-free-group-start over = if ( size index end )
  r@ group-size@ swap r> group-size@ + <= ( flag )
  else
  rdrop 2drop drop false ( flag )
  then
;

\ Initialize a heap's bitmap
: init-heap-bitmap ( heap -- )
  dup heap-bitmap swap heap-block-count @ 3 rshift 0 fill
;

\ Find a free group
: find-free ( size heap -- index )
  dup >r heap-next-free @
  begin dup -1 <> while
    dup r@ group-size@ 2 pick >= if
      rdrop nip exit
    else
      r@ group-next-free@
    then
  repeat
  rdrop nip
;

\ Allocate from a group
: allocate-from-group ( size index heap -- )
  >r dup r@ group-size@ 2 pick = if
  dup r@ group-prev-free@ dup -1 <> if
  over r@ group-next-free@ swap r@ group-next-free!
  else
  drop dup r@ group-next-free@ r@ heap-next-free!
  then
  dup r@ group-next-free@ dup -1 <> if
  over r@ group-prev-free@ swap r@ group-prev-free!
  else
  drop
  then
  ( size index )
  else
  dup r@ group-prev-free@ dup -1 <> if
  >r 2dup + r> r@ group-next-free!
  else
  drop 2dup + r@ heap-next-free!
  then
  dup r@ group-next-free@ dup -1 <> if
  >r 2dup + r> r@ group-prev-free!
  else
  drop
  then
  dup r@ group-prev-free@ 2 pick 2 pick + r@ group-prev-free!
  dup r@ group-next-free@ 2 pick 2 pick + r@ group-next-free!
  ( size index )
  dup r@ group-size@ 2 pick - ( size index new-size )
  2 pick 2 pick + r@ group-size! ( size index )
  2dup r@ group-size! ( size index )
  then
  r> mark-allocated ( )
;

\ Expand a group
: expand-group ( size index heap -- )
  >r dup r@ group-end@ ( size index end )
  over r@ group-size@ 3 pick - ( size index end size-diff )
  over r@ group-size@ + ( size index end new-end-size )
  dup 0<> if
  r> swap >r >r -rot tuck r@ group-size! ( end index )
  dup r@ group-size@ over r@ mark-allocated swap ( index end )
  dup r@ group-prev-free@ ( index end prev-group )
  swap r@ group-next-free@ ( index prev-group next-group )
  rot r@ group-end@ ( prev-group next-group new-end )
  r> r> swap >r over r@ group-size! ( prev-group next-group new-end )
  2dup r@ group-next-free! ( prev-group next-group new-end )
  over -1 <> if
  tuck swap r@ group-prev-free! ( prev-group new-end )
  else
  nip ( prev-group new-end )
  then
  2dup r@ group-prev-free! ( prev-group new-end )
  over -1 <> if
  swap r> group-next-free! ( )
  else
  nip r> heap-next-free! ( )
  then
  else
  drop -rot tuck r@ group-size! ( end index )
  dup r@ group-size@ swap r@ mark-allocated ( end )
  dup r@ group-prev-free@ ( end prev-group )
  swap r@ group-next-free@ ( prev-group next-group )
  dup -1 <> if
  2dup r@ group-prev-free! ( prev-group next-group )
  then
  over -1 <> if
  swap r> group-next-free! ( )
  else
  nip r> heap-next-free! ( )
  then
  then
;

\ Verify that memory is allocated
: is-allocated? ( index size heap -- allocated? )
  >r swap tuck + begin 2dup < while
  1- dup r@ block-allocated? not if rdrop 2drop false exit then
  repeat
  rdrop 2drop true
;

\ Verify that the first free block is actually the first free block
: verify-first-block ( heap -- )
  drop
;

\ Verify that no memory is allocated after the last free block
: verify-last-block ( heap -- )
  drop
;

\ Dump a heap's bitmap
: diagnose-heap ( heap -- )
  cr ." DIAGNOSTIC: "
  cr ." Heap start: " 0 over heap-block hex.
  \    dup heap-next-free @ begin dup -1 <> while
  \      cr ." Free block: Index: " dup .
  \      ." Size: " 2dup swap group-size@ .
  \      2dup swap group-next-free@ over <> if \ DEBUG
  \  over group-next-free@
  \      else \ DEBUG
  \  2drop -1 \ DEBUG
  \      then \ DEBUG
  \    repeat
  \    drop cr
  cr
  dup heap-block-count @ 0 ?do
  i over block-allocated? if ." *" else ." ." then
  loop
  drop
;

\ Initialize a heap at a given address with a given block size and block count
: init-heap ( block-size block-count addr -- )
  tuck swap 32 alignto swap heap-block-count !
  tuck swap cell alignto 3 cells max swap heap-block-size !
  0 over heap-next-free!
  dup heap-block-count @ 0 2 pick group-size!
  -1 0 2 pick group-prev-free!
  -1 0 2 pick group-next-free!
  init-heap-bitmap
;

\ Allocate from a heap
: allocate ( bytes heap -- addr )
  >r cell+ r@ size>blocks ( size )
  dup r@ find-free dup -1 <> averts x-allocate-failed ( size index )
  tuck r@ allocate-from-group ( index size index )
  r@ heap-block cell+ ( addr )
  r@ verify-first-block
  r> verify-last-block
;

\ Free a group in a heap
: free ( addr heap -- )
  >r cell - r@ block-addr>index ( index )
  dup r@ group-size@ ( index size )
  swap dup r@ link-group ( size index )
  r@ mark-free ( )
  r@ verify-first-block
  r> verify-last-block
;

\ Resize a group in a heap
: resize ( bytes addr heap -- new-addr )
  >r cell - r@ block-addr>index ( bytes index )
  swap cell+ r@ size>blocks swap ( size index )
  2dup r@ group-size@ > if
    2dup r@ expandable-group? if ( size index )
      tuck r@ expand-group r> heap-block cell+ ( new-addr )
    else
      swap r@ heap-block-size @ * cell - r@ allocate ( index new-addr )
      over r@ heap-block cell+ swap ( index addr new-addr )
      2 pick r@ group-size@ r@ heap-block-size @ * cell -
      ( index addr new-addr bytes )
      over >r move r> ( index new-addr )
      swap r@ heap-block cell+ r> free ( new-addr )
    then
  else ( size index )
    nip r> heap-block cell+ ( same-addr )
  then
;

\ Get the size of a heap with a given block size and block count
: heap-size ( block-size block-count -- heap-bytes )
  32 alignto swap cell alignto 3 cells max over * swap 5 rshift cells +
  heap-size +
;

