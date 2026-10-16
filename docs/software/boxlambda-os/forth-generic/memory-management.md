# Memory Management

## ZeptoForth Heap

[heap.fs](../../../../fs/forth/boxkern-includes/heap.fs)

Heaps are created by users and consist of discrete blocks that are allocated, freed, and resized in multiples. The size of an allocation, plus a cell used for storing the block count, is rounded up to the next full number of blocks. No global heap exists. Note that the time required for heap allocation or resizing is bounded by a maximum defined by the number of blocks in the heap; any heap allocation or resizing may take this maximal time. In contrast, the time required for freeing an allocation is determined solely by the number of blocks comprising the allocation.

`heap-size ( block-size block-count -- heap-bytes )`

- Determine the size in bytes of a heap with the given block size and block count.

`init-heap ( block-size block-count addr -- )`

- Initialize a heap at _addr_ with the given block size in bytes and block count; note that the size of the available memory at _addr_ should be equal to or greater than the number of bytes returned by `heap-size` for _block-size_ and _block-count_.

`allocate ( size heap -- addr )`

- Allocate memory in a heap of _size_ bytes and return its address; if the memory cannot be allocated due to insufficient contiguous memory being available, `x-allocate-failed` is raised.

`free ( addr heap -- )`

- Free memory at _addr_ in a heap.

`resize ( size addr heap -- new-addr )`

- Resize memory in a heap at _addr_ to a new size in bytes, returning its new address. If sufficient memory is available for resizing at _addr_ the allocation is expanded without moving or copying it and _addr_ is returned. Otherwise, the allocation at _addr_ is freed, and its contents are copied to a new allocation, whose address is returned. Note that if insufficient memory is available in the heap for resizing the allocation, the existing allocation is preserved, and `x-allocate-failed` is raised.

Exceptions:

```
x-allocate-failed
x-internal-error
x-memory-not-allocated
```

## ZeptoForth Pool

[pool.fs](../../../../fs/forth/boxkern-includes/pool.fs)

Pools are created by users and consist of discrete blocks that are allocated and freed as a single unit. By default, there is no global pool. Allocating and freeing blocks in pools occur in constant time and are fast, unlike allocation, resizing, and freeing in heaps.

`pool-size ( -- bytes )`

- Get the size of a pool header in bytes.

`init-pool ( block-size addr -- )`

- Initialize a pool at _addr_ with the given block size of _block-size_ bytes. Note that no space for storing blocks is available in a pool when it is first initialized; to add memory to a pool, use `add-pool`.

`add-pool ( addr bytes pool -- )`

- Add memory starting at _addr_ of size _bytes_ to _pool_ as discrete blocks; only a multiple of the block size of the pool will be added to the pool, so if _bytes_ is not a multiple of said block size not all of the space in the memory provided will be used.

`allocate-pool ( pool -- addr )`

- Allocate a block in _pool_ and return its address. If no blocks are available in the pool, `x-allocate-failed` is raised.

`free-pool ( addr pool -- )`

- Free a block at _addr_ in _pool_, making it available to future allocation.

`pool-block-size ( pool -- bytes )`

- Get the block size of a pool.

`pool-free-count ( pool -- count )`

- Get the number of free blocks in a pool.

`pool-total-count ( pool -- count )`

- Get the total number of blocks in a pool.

Exceptions:

```
x-allocate-failed
```

## Temporary Memory Allocator

[temp-alloc.fs](../../../../fs/forth/boxkern-includes/temp-alloc.fs)

`temp-allot ( u -- )`

- Allot u bytes from temporary buffer may throw `x-temp-allot-failed`.

`temp-mark> ( -- mark )`

- Get temp allocator mark. The mark can be used as an address.

`>temp-mark ( mark -- )`

- Revert to mark

`with-temp-allot ( u xt -- )`

- Execute xt, passing in a buffer of u bytes at TOS. Release buffer when xt has completed.

Example:

```
: printf ( n*x c-addr u -- ) 256 [: sprintf type ;] with-temp-allot ;
```

Exceptions:

```
x-temp-allot-failed
```


