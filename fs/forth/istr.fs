\ BoxLambda Interpretive string support.

\ String pool: 256 addr-len pairs:

begin-structure str-pool-entry
  field: .addr
  field: .len
end-structure

\ Pool header:
create str-pool pool-size allot
str-pool-entry str-pool init-pool

\ The actual pool memory:
256 str-pool-entry * constant str-pool-mem-sz
create str-pool-mem str-pool-mem-sz allot
\ O-initialize it before adding it to the pool.
\ Note that add-pool will hijack the first cell
\ of each element to build a linked list.
str-pool-mem str-pool-mem-sz 0 fill
str-pool-mem str-pool-mem-sz str-pool add-pool

\ Max. number of strings constant (256)
str-pool pool-total-count constant max-strings

\ The string pool contains the addr-len pairs.
\ We also need memory to store the strings themselves.
\ The string heap provides this memory.
\ 4096 16 byte blocks:
create str-heap 16 4096 heap-size allot
16 4096 str-heap init-heap

\ Use the length to decide whether or not this entry is in use. The
\ addr field is kept in the lower memory word of each pool element,
\ which is used as a link field for unallocated entries, i.e. the addr
\ field is only valid on allocated entries.
\ ( str-pool-ptr -- f )
: istr-allocated? .len @ 0<> ;

\ ( pool-ptr -- addr len )
: pool-ptr>str dup .addr @ swap .len @ ;

\ Dump the string table
\ Note that only strings created in interpreter mode
\ end up in this string table.
\ Compiled strings are stored at their compilation location
\ inside the dictionary.
\ ( -- )
: istr-dump
  cr
  \ Iterate over the string pool.
  str-pool-mem
  max-strings 0 do ( pool-ptr )
    dup istr-allocated? if ( pool-ptr )
      \ Print entry, starting with index. This index can be used to
      \ release strings from the pool (istr-free).
      dup pool-ptr>str ( pool-ptr addr len )
      i . space type cr ( pool-ptr )
    then
    str-pool-entry +
  loop
  drop
  cr
;

\ Free (release) a string in the string table.
\ ( string-tbl-idx -- )
: istr-free
  str-pool-entry * str-pool-mem + ( pool-ptr ) \ Convert to pool-ptr
  dup istr-allocated? if ( pool-ptr ) \ Only act on allocated strings.
    dup .addr @ str-heap free ( pool-ptr ) \ Release the string from the string heap.
    \ Set length field to 0 to indicate this entry has been freed.
    dup .len 0 swap ! ( pool-ptr )
    dup str-pool free-pool ( pool-ptr ) \ Release the string pool entry.
  then
  drop ( )
;

\ Enter string into the string pool.
\ ( addr len -- )
: set-str-pool-entry
  str-pool allocate-pool ( addr len pool-ptr )
  tuck ( addr pool-ptr len pool-ptr )
  .len ! .addr !
;

\ Allocate memory on string heap and store string in it.
\ ( addr len -- addr )
: store-str-heap
  >r ( addr )
  r@ str-heap allocate ( addr heap-addr )
  tuck r> ( heap-addr addr heap-addr len )
  move ( heap-addr )
;

\ Allocate heap memory and string-pool entry, store given string in it, and return
\ the new location's addr/len pair.
\ ( addr len -- addr' len )
: alloc-store-str
  tuck ( len addr len )
  store-str-heap ( len addr' ) \ Allocate heap memory and store string in heap
  swap 2dup set-str-pool-entry ( addr' len ) \ Store string addr/len in string-pool
;

\ Compile or interpret a string and give back its address and length when executed.
\ ( -- addr len )
: s"
  state @ if
    postpone s" \ Compiled string.
  else
    [char] " parse ( addr len )
    alloc-store-str
  then
  [immediate]
;

