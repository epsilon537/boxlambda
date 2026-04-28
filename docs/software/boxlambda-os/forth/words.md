# Forth Word List

I decided to place all Forth Words on a single page for easy reference. Some Words will require more context than a one-liner can provide.
In such cases, the one-liner contains a link to a page with additional info.

Most of these Words are not original to me; they come from Mecrisp Forth, ZeptoForth, and other resources. I would like to give credit where it's due, but doing so for each Word on this already crowded page seems impractical. The Forth source code contains references to their origins. In some cases, I have included the origin in the title, such as [ZeptoForth Heap](zeptoforth-heap).

To understand how the various software modules referenced below are related to each other, examine the [stack](fs-stack.md).

## Constants and Units

[units.fs](../../../../fs/forth/units.fs)

`cell` size in bytes.

`max-uint`

- Maximum value for an unsigned integer.

`max-int`

- Maximum value for a signed integer.

`min-int`

- Minimum value for a signed integer.

`cells+  ( x n -- x + n * Cell )`

- Add the size of `n` cells to `x`.

`chars  ( u -- u )`

- Returns the size of `u` characters, which is equal to `u`. Used for clarity, to put a unit behind a number. For example, `8 Chars`.

`char  ( a -- a + 1 )`

- Increment `a` by one character.

## Range Related Words

[range.fs](../../../../fs/forth/range.fs)

`span ( addr len -- start end )`

- Convert addr len to start-end address span.

`bounds ( addr len -- end start )`

- Convert addr len to end-start address span.

`within ( n low high -- flag )`

- True if n is within the range [low..high[.

## Arrays

[array.fs](../../../../fs/forth/array.fs)

`array <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n cells with given name. `i <name>` returns the address of the i-th cell.

`carray <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n bytes with given name. `i <name>` returns the address of the i-th byte.

## ZeptoForth Exceptions

[exception.s](../../../../sw/components/forth/exception.s)
[except.fs](../../../../fs/forth/except.fs)

See [Forth Exception Handling](exception-handling.md).

`?raise ( xt|0 -- | 0 )`

- Raise exception xt if non-zero.

`try ( xt1 -- xt2|0 )`

- Execute xt1. Catch and return exception raised. Return 0 if no exception was raised.

`averts ( f "exception name" -- )`

- Assert that a value is true, otherwise raise a specified exception.

`triggers ( f "exception name" -- )`

- Assert that a value is false, otherwise raise a specified exception.

`suppress ( exc|0 "exception name" -- exc|0 )`

 Check if an exception, typically returned by try, matches a specified exception. If it does, replace it with zero to indicate no exception. Otherwise, pass the specified argument through.

`x-assert ( -- )`

- Assert exception.

`?assert ( f -- )`

- Raise `x-assert` exception if f is false.

Example:
```
: x-spc-exception ." Don't press space!" ;

: foo-no-catch
  ." Press any key except space." cr key bl = triggers x-spc-exception
  ." Thank you for not pressing space" cr
;

: foo-w-catch
  [: ." Press any key except space." cr key bl = triggers x-spc-exception ;] try
  ?dup if
    ." Exception caught. Rethrowing..." cr
    ?raise
  else
    ." Thank you for not pressing space. " cr
  then
;
```

## ZeptoForth Lambda Anonymous Functions

[lambda.fs](../../../../fs/forth/lambda.fs)

`[: ( -- )`

- Immediate. Begin Lambda.

`;] ( compile-time: -- ) ( run-time: -- xt )`

- Compile-only Immediate. End Lambda definition. At run-time, returns the *xt* of the code between `[:` and `:]`.

Example 1:
```
  token
  [: 2dup f_stat ;] try 0=
```

Example 2:
```
: rm
  cr
  token
  [: ( patha pathl )
    2dup s" Removing: %s" printf cr ( patha pathl )
    f_unlink ( dir )
  ;] ( pata patl xt )
  glob-each
;
```

## ZeptoForth Structs

[struct.fs](../../../../fs/forth/struct.fs)

`begin-structure ( "name" -- addr offset )`

- Begin declaring a structure.

`end-structure ( addr offset -- )`

- Finish declaring a structure.

`+field: ( offset size "name" -- offset )`

- Create an arbitrary-sized field.

`cfield: ( offset "name" -- offset )`

- Create a byte-sized field.

`hfield: ( offset "name" -- offset )`

- Create a halfword-sized field.

`field: ( offset "name" -- offset )`

- Create a cell-sized field.

`2field: ( offset "name" -- offset )`

- Create a double cell-sized field.

Example:
```
begin-structure fil-buf
  field: .fil
  field: .buf
end-structure

create fil-buf0 fil-buf allot
create fil-buf1 fil-buf allot

fil-buf0 .fil @ fil-buf0 .buf @ 256 f_read
```

## ZeptoForth Heap

[heap.fs](../../../../fs/forth/heap.fs)

Heaps are created by users and consist of discrete blocks that are allocated, freed, and resized in multiples. The size of an allocation, plus a cell used for storing the block count, is rounded up to the next full number of blocks. No global heap exists. Note that the time required for heap allocation or resizing is bounded by a maximum defined by the number of blocks in the heap; any heap allocation or resizing may take this maximal time. In contrast, the time required for freeing an allocation is determined solely by the number of blocks comprising the allocation.

`heap-size ( block-size block-count -- heap-bytes )`

- Determine the size in bytes of a heap with the given block size and block count.

`init-heap ( block-size block-count addr -- )`

- Initialize a heap at *addr* with the given block size in bytes and block count; note that the size of the available memory at *addr* should be equal to or greater than the number of bytes returned by `heap-size` for *block-size* and *block-count*.

`allocate ( size heap -- addr )`

- Allocate memory in a heap of *size* bytes and return its address; if the memory cannot be allocated due to insufficient contiguous memory being available, `x-allocate-failed` is raised.

`free ( addr heap -- )`

- Free memory at *addr* in a heap.

`resize ( size addr heap -- new-addr )`

- Resize memory in a heap at *addr* to a new size in bytes, returning its new address. If sufficient memory is available for resizing at *addr* the allocation is expanded without moving or copying it and *addr* is returned. Otherwise, the allocation at *addr* is freed, and its contents are copied to a new allocation, whose address is returned. Note that if insufficient memory is available in the heap for resizing the allocation, the existing allocation is preserved, and `x-allocate-failed` is raised.

Exceptions:
```
x-allocate-failed
x-internal-error
x-memory-not-allocated
```

## ZeptoForth Pool

[pool.fs](../../../../fs/forth/pool.fs)

 Pools are created by users and consist of discrete blocks that are allocated and freed as a single unit. By default, there is no global pool. Allocating and freeing blocks in pools occur in constant time and are fast, unlike allocation, resizing, and freeing in heaps.

`pool-size ( -- bytes )`

- Get the size of a pool header in bytes.

`init-pool ( block-size addr -- )`

- Initialize a pool at *addr* with the given block size of *block-size* bytes. Note that no space for storing blocks is available in a pool when it is first initialized; to add memory to a pool, use `add-pool`.

`add-pool ( addr bytes pool -- )`

- Add memory starting at *addr* of size *bytes* to *pool* as discrete blocks; only a multiple of the block size of the pool will be added to the pool, so if *bytes* is not a multiple of said block size not all of the space in the memory provided will be used.

`allocate-pool ( pool -- addr )`

- Allocate a block in *pool* and return its address. If no blocks are available in the pool, `x-allocate-failed` is raised.

`free-pool ( addr pool -- )`

- Free a block at *addr* in *pool*, making it available to future allocation.

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

[temp-alloc.fs](../../../../fs/forth/temp-alloc.fs)

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

## Terminal-IO

[terminal.s](../../../../sw/components/forth/terminal.s)
[terminalhooks.s](../../../../sw/components/forth/terminalhooks.s)

`emit? ( -- Flag )`

- Ready to send a character?

`key? ( -- Flag )`

- Checks if a key is waiting.

`key ( -- Char )`

- Waits for and fetches the pressed key.

`emit ( Char -- )`

- Emits a character.

```
hook-emit? ( -- a-addr )
hook-key? ( -- a-addr )
hook-key ( -- a-addr )
hook-emit ( -- a-addr )
```

- Hooks for redirecting terminal IO on the fly.

```
serial-emit? ( -- Flag )
serial-key? ( -- Flag )
serial-key ( -- Char )
serial-emit ( Char -- )
```

- Serial interface terminal routines as default communications.

`hook-pause ( -- a-addr )`

- Hook for a multitasker.

`pause ( -- )`

- Task switch, none for default.

## Stack Jugglers

[stackjugglers.s](../../../../sw/components/forth/stackjugglers.s)
[utils.fs](../../../../fs/forth/utils.fs)

### Single-Jugglers

`depth ( -- +n )`

- Gives number of single-cell stack items.

`nip ( x1 x2 -- x2 )`

`drop ( x -- )`

`rot ( x1 x2 x3 -- x2 x3 x1 )`

`-rot ( x1 x2 x3 -- x3 x1 x2 )`

`swap ( x1 x2 -- x2 x1 )`

`tuck ( x1 x2 -- x2 x1 x2 )`

`over ( x1 x2 -- x1 x2 x1 )`

`?dup ( x -- 0 | x x )`

`dup ( x -- x x )`

`pick ( ... xi+1 xi ... x1 x0 i -- ... x1 x0 xi )`

- Picks one element from deep below.

`roll ( xu ... x0 u -- xu-1 ... x0 xu )`

- Roll takes the element u deep in the stack and moves it to the top, shifting the elements above it down by one.

`>r ( x -- ) (R: -- x )`

`r> ( -- x ) (R: x -- )`

`r@ ( -- x ) (R: x -- x )`

`rdrop (  --  ) (R: x -- )`

`rdepth ( -- +n ) Gives number of return stack items.`

`rpick ( i -- xi ) R: ( ... xi ... x0 -- ... xi ... x0 )`

### Double-Jugglers

[double.s](../../../../sw/components/forth/double.s)

`2nip ( x1 x2 x3 x4 -- x3 x4 )`

`2drop ( x1 x2 -- )`

`2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )`

`2-rot ( x1 x2 x3 x4 x5 x6 -- x5 x6 x1 x2 x3 x4 )`

`2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )`

`2tuck ( x1 x2 x3 x4 -- x3 x4 x1 x2 x3 x4 )`

`2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )`

`2dup ( x1 x2 -- x1 x2 x1 x2 )`

`2>r ( x1 x2 -- ) (R: -- x1 x2 )`

`2r> ( -- x1 x2 ) (R: x1 x2 -- )`

`2r@ ( -- x1 x2 ) (R: x1 x2 -- x1 x2 )`

`2rdrop ( -- ) (R: x1 x2 -- )`

### Stack pointers

[stackjugglers.s](../../../../sw/components/forth/stackjugglers.s)

`sp@ ( -- a-addr )`

- Fetch data stack pointer.

`sp! ( a-addr -- )`

- Store data stack pointer.

`rp@ ( -- a-addr )`

- Fetch return stack pointer.

`rp! ( a-addr -- )`

- Store return stack pointer.

## Logic and Bit Manipulation

[logic.s](../../../../sw/components/forth/logic.s)
[utils.fs](../../../../fs/forth/utils.fs)

Shifts decode the lowest 5 bits only on RISC-V. Therefore, ar/r/lshift behaves like "31 and ar/r/lshift". 32 lshift does nothing.

`arshift ( x1 u -- x2 )`

- Arithmetic right-shift of `u` bit-places.

`rshift ( x1 u -- x2 )`

- Logical right-shift of `u` bit-places.

`lshift ( x1 u -- x2 )`

- Logical  left-shift of `u` bit-places.

`shr ( x1 -- x2 )`

- Logical right-shift of one bit-place.

`shl ( x1 -- x2 )`

- Logical  left-shift of one bit-place.

`ror ( x1 -- x2 )`

- Logical right-rotation of one bit-place.

`rol ( x1 -- x2 )`

- Logical  left-rotation of one bit-place.

`bitval ( u -- u' )`

- Integer value corresponding to bit position u (i.e., 1<<u).

`bic ( x1 x2 -- x3 )`

- Bit clear, identical to "not and".

`not ( x1 -- x2 )`

- Invert all bits.

`xor ( x1 x2 -- x3 )`

- Bitwise Exclusive-OR.

`or ( x1 x2 -- x3 )`

- Bitwise OR.

`and ( x1 x2 -- x3 )`

- Bitwise AND.

`false ( --  0 )`

- False flag.

`true ( -- -1 )`

- True flag.

`clz ( x1 -- u )`

- Count leading zeros.

## Calculus

### Single Number Calculus (exactly ANS, some logical extensions)

[multiplydivide.s](../../../../sw/components/forth/multiplydivide.s)
[calculations.s](../../../../sw/components/forth/calculations.s)

`u/mod ( u1 u2 -- u3 u4 )`

- 32/32 = 32 rem 32 Division. u1 / u2 = u4 remainder u3.

`/mod ( n1 n2 -- n3 n4 )`

- n1 / n2 = n4 rem n3.

`mod ( n1 n2 -- n3 )`

- n1 / n2 = remainder n3.

`/ ( n1 n2 -- n3 )`

- n1 / n2 = n3.

`* ( u1|n1 u2|n2 -- u3|n3 )`

- 32*32 = 32 Multiplication.

`2- ( u1|n1 -- u2|n2 )`

- Subtracts two, optimized.

`1- ( u1|n1 -- u2|n2 )`

- Subtracts one, optimized.

`2+ ( u1|n1 -- u2|n2 )`

- Adds two, optimized.

`1+ ( u1|n1 -- u2|n2 )`

- Adds one, optimized.

`even ( u1|n1 -- u2|n2 )`

- Makes even. Adds one if uneven.

`2* ( n1 -- n2 )`

- Arithmetic  left-shift.

`2/ ( n1 -- n2 )`

- Arithmetic right-shift.

`abs ( n -- u )`

- Absolute value.

`negate ( n1 -- n2 )`

- Negate.

`- ( u1|n1 u2|n2 -- u3|n3 )`

- Subtraction.

`+ ( u1|n1 u2|n2 -- u3|n3 )`

- Addition.

### Double Number Calculus (exactly ANS, some logical extensions)

[double.s](../../../../sw/components/forth/double.s)
[multiplydivide.s](../../../../sw/components/forth/multiplydivide.s)
[utils.fs](../../../../fs/forth/utils.fs)

`um+ ( u1 u2 -- u carry )`

- Unsigned addition with carry.

`um* ( u1 u2 -- ud )`

- 32*32 = 64 Multiplication.

`ud* ( ud1|d1 ud2|d2 -- ud3|d3 )`

- 64*64 = 64 Multiplication.

`udm* ( ud1 ud2 -- ud3-Low ud4-High )`

- 64*64=128 Multiplication.

`um/mod ( ud u1 -- u2 u3 )`

- ud / u1 = u3 remainder u2.

`ud/mod ( ud1 ud2 -- ud3 ud4 )`

- 64/64 = 64 rem 64 Division ud1 / ud2 = ud4 remainder ud3.

`m+ ( d n -- d' )`

- Add n to d.

`m* ( n1 n2 -- d )`

- n1 * n2 = d.

`m/mod ( d  n1 -- n2 n3 )`

- d  / n1 = n3 remainder r2.

`d/mod ( d1 d2 -- d3 d4 )`

- d1 / d2 = d4 remainder d3.

`d/ ( d1 d2 -- d3 )`

- d1 / d2 = d3.

`*/ ( n1 n2 n3 -- n4 )`

- n1 * n2 / n3 = n4.

`u*/ ( u1 u2 u3 -- u4 )`

- u1 * u2 / u3 = u4.

`*/mod ( n1 n2 n3 -- n4 n5 )`

- n1 * n2 / n3 = n5 remainder n4.

`u*/mod ( u1 u2 u3 -- u4 u5 )`

- u1 * u2 / u3 = u5 remainder u4.

`d2* ( d1 -- d2 )`

- Arithmetic  left-shift.

`d2/ ( d1 -- d2 )`

- Arithmetic right-shift.

`dshl ( ud1 -- ud2 )`

- Logical  left-shift, same as d2*.

`dshr ( ud1 -- ud2 )`

- Logical right-shift.

`dabs ( d -- ud )`

- Absolute value.

`dnegate ( d1 -- d2 )`

- Negate.

`d- ( ud1|d1 ud2|d2 -- ud3|d3 )`

- Subtraction.

`d+ ( ud1|d1 ud2|d2 -- ud3|d3 )`

- Addition.

`s>d ( n -- d )`

- Makes a signed single number double-length.

`2arshift ( d1 u -- d2 )`

- Arithmetic double right-shift of u bit-places.

`2rshift ( d1 u -- d2 )`

- Logical double right-shift of u bit-places.

`2lshift ( d1 u -- d2 )`

- Logical double left-shift of u bit-places.

## Fixed-Point numbers

S31.32 fixed-point numbers are written like `3,14159`, i.e., written with a comma
instead of a dot.

Fixed-point numbers are stored ( n-comma n-whole ) and can be handled
like signed double numbers.

[double.s](../../../../sw/components/forth/double.s)

`f/ ( df1 df2 -- df3 )`

- Division of two fixed-point numbers.

`f* ( df1 df2 -- df3 )`

- Multiplication.

## Comparisons  (exactly ANS, some logical extensions)

### Single Comparisons

[comparisons.s](../../../../sw/components/forth/comparisons.s)
[utils.fs](../../../../fs/forth/utils.fs)

`u<= ( u1 u2 -- flag )`

- Unsigned comparisons.

`u>= ( u1 u2 -- flag )`

`u> ( u1 u2 -- flag )`

`u< ( u1 u2 -- flag )`

`<= ( n1 n2 -- flag )`

- Signed comparisons.

`>= ( n1 n2 -- flag )`

`> ( n1 n2 -- flag )`

`< ( n1 n2 -- flag )`

`0< ( n - flag )`

- Negative?

`0> ( n - flag )`

- Positive?

`0<> ( x -- flag )`

`0= ( x -- flag )`

`<> ( x1 x2 -- flag )`

`= ( x1 x2 -- flag )`

`min ( n1 n2 -- n1|n2 )`

- Keeps smaller of top two items.

`max ( n1 n2 -- n1|n2 )`

- Keeps greater of top two items.

`umin ( u1 u2 -- u1|u2 )`

- Keeps unsigned smaller.

`umax ( u1 u2 -- u1|u2 )`

- Keeps unsigned greater.

### Double-Comparisons

[double.s](../../../../sw/components/forth/double.s)

`du> ( ud1 ud2 -- flag )`

`du< ( ud1 ud2 -- flag )`

`d> ( d1 d2 -- flag )`

`d< ( d1 d2 -- flag )`

`d0< ( d -- flag )`

`d0= ( d -- flag )`

`d<> ( d1 d2 -- flag )`

`d= ( d1 d2 -- flag )`

### Specials

[calculation.s](../../../../sw/components/forth/calculations.s)

`slt ( u1 u2 -- 0 | 1 )`

- Set if less than.

`sltu ( u1 u2 -- 0 | 1 )`

- Set if less than, unsigned.

## Number base

[calculation.s](../../../../sw/components/forth/calculations.s)

`binary ( -- )`

- Sets base to 2.

`decimal ( -- )`

- Sets base to 10.

`hex ( -- )`

- Sets base to 16.

`base ( -- a-addr )`

- Base variable address.

## Memory access

[memory.s](../../../../sw/components/forth/memory.s)
[compiler.s](../../../../sw/components/forth/compiler.s)
[compiler-memory.s](../../../../sw/components/forth/compiler-memory.s)
[double.s](../../../../sw/components/forth/double.s)

`move ( c-addr1 c-addr2 u -- )`

- Moves u bytes in memory.

`fill ( c-addr u c )`

- Fill u bytes of memory with value c.

`cbit@ ( mask c-addr -- flag )`

- Test bits in byte location.

`hbit@ ( mask h-addr -- flag )`

- Test bits in halfword location.

`bit@ ( mask a-addr -- flag )`

- Test bits in word location.

`cxor! ( mask c-addr -- )`

- Toggle bits in byte location.

`hxor! ( mask h-addr -- )`

- Toggle bits in halfword location.

`xor! ( mask a-addr -- )`

- Toggle bits in word location.

`cbic! ( mask c-addr -- )`

- Clear bits in byte location.

`hbic! ( mask h-addr -- )`

- Clear bits in halfword location.

`bic! ( mask a-addr -- )`

- Clear bits in word location.

`cbis! ( mask c-addr -- )`

- Set bits in byte location.

`hbis! ( mask h-addr -- )`

- Set bits in halfword location.

`bis! ( mask a-addr -- )`

- Set bits in word location.

`2constant name  ( ud|d -- )`

- Makes a double constant.

`constant name  ( u|n -- )`

- Makes a single constant.

`2variable name  ( ud|d -- )`

- Makes an initialized double variable.

`variable name  ( n|n -- )`

- Makes an initialized single variable.

`nvariable name  ( n1*u|n n1 -- )`

- Makes an initialized variable with specified size of n1 words. Maximum is 15 words.

`buffer: name    ( u -- )`

- Creates a buffer in RAM with u bytes length.

`2@ ( a-addr -- ud|d )`

- Fetches double number from memory.

`2! ( ud|d a-addr -- )`

- Stores double number in memory.

`@ ( a-addr -- u|n )`

- Fetches single number from memory.

`! ( u|n a-addr -- )`

- Stores single number in memory.

`+! ( u|n a-addr -- )`

- Add to memory location.

`h@ ( h-addr -- u )`

- Fetches halfword from memory.

`h@signed ( h-addr -- n )`

- Fetches halfword with sign extension.

`h! ( u h-addr )`

- Stores halfword in memory.

`h+! ( u|n h-addr -- )`

- Add to halfword memory location.

`c@ ( c-addr -- char )`

- Fetches byte from memory.

`c@signed ( c-addr -- n )`

- Fetches byte with sign extension.

`c! ( char c-addr )`

- Stores byte in memory.

`c+! ( u|n a-addr -- )`

- Add to byte memory location.

## Comments

[strings.s](../../../../sw/components/forth/strings.s)

`( Comment )`

- Ignore Comment.

`\ Comment`

- Comment to end of line.

## Strings and formatted output (exactly ANS, some logical extensions)

### String routines

[strings.s](../../../../sw/components/forth/strings.s)
[numberstrings.s](../../../../sw/components/forth/numberstrings.s)
[numberoutput.s](../../../../sw/components/forth/numberoutput.s)
[istr.fs](../../../../fs/forth/istr.fs)
[utils.fs](../../../../fs/forth/utils.fs)
[tonumber.fs](../../../../fs/forth/tonumber.fs)

`type ( c-addr length -- )`

- Prints a string.

`s" text"       ( -- c-addr length )`

- Core: Compile-only Immediate. Compiles the string given on input stream. The compiled code, when executed, puts the string address and length on the data stack.

- `istr.fs`: Adds interpretive execution mode behavior. Take string data from input stream, put string address and length on the data stack.

`." text" ( -- )`

- Core: Compiles a string to be printed when executed.

- `istr.fs`: Adds interpretive execution mode behavior. Take string data from input stream and print it.

`cr ( -- )`

- Emits line feed.

`bl ( -- 32 )`

- ASCII code for Space.

`space ( -- )`

- Emits space.

`spaces ( n -- )`

- Emits n spaces if n is positive.

`compare ( caddr-1 len-1 c-addr-2 len-2 -- flag )`

- Compares two strings.

`/string (addr u n -- addr' u')`

- Move string pointer forward by n and reduce string length by n.

```
number ( c-addr length -- 0 )
                       -- n 1 )
                       -- n-low n-high 2 )
```

- Tries to convert a string to a number.

`>digit ( char -- d true | 0 )`

- char to digit.

`>number ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )`

- ud2 is the unsigned result of converting the characters within the string
specified by c-addr1 u1 into digits, using the number in `base`, and adding
each into ud1 after multiplying ud1 by the number in `base`. Conversion
continues left-to-right until a character that is not convertible, including
any "+" or "-", is encountered or the string is entirely converted. c-addr2 is
the location of the first unconverted character or the first character past the
end of the string if the string was entirely converted. u2 is the number of
unconverted characters in the string.

### Counted string routines

`ctype ( cstr-addr -- )`

- Prints a counted string.

`c" Hello"       ( -- cstr-addr )`

- Compiles a counted string and gives back its address when executed.

`cexpect ( cstr-addr maxlength -- )`

- Read input into a counted string.

`count ( cstr-addr -- c-addr length )`

- Convert counted string into addr-length string.

### Escaped Strings

[escstr.fs](../../../../fs/forth/escstr.fs)

`escape-string ( addr len -- addr' len' )`

- In given string, substitute escape codes according to table below.

`esc-s" ( parses up to "  -- addr len )`

- Parse string from input string an substitute escape codes according to table below.

`.esc-s" ( parses up to "  -- )`
Parse string from input string, substitute escape codes according to table below, and print.

| Escape Code | Substitution |
|-------------|--------------|
| \t | Tab |
| \n | LF  |
| \r | CR  |
| \e | ESC |
| \\ | \   |
| '  | "   |

### Formatted Numerical Output

`.digit ( u -- char )`

- Converts a digit to a char.

`digit ( char -- u true | false )`

- Converts a char to a digit.

`[char] "<char>" ( compile time: "<char>" -- ) ( run time: -- <char> )`

- Compile-only Immediate. Compiles ASCII code of input-stream-following char. When the compiled code executes, the ASCII code is put on the data stack.

`char "<char>" ( -- <char> )`

- Take one character from the input stream and put its ASCII code on the data stack.

`hold ( char -- )`

- Adds character to pictured number output buffer from the front.

`sign ( n -- )`

- Add a minus sign to pictured number output buffer, if n is negative.

`#S ( ud1|d1 -- 0 0 )`

- Add all remaining digits from the double-length number to output buffer.

`# ( ud1|d1 -- ud2|d2 )`

- Add one digit from the double-length number to output buffer.

`#> ( ud|d -- c-addr len )`

- Drops double-length number and finishes pictured numeric output ready for type.

`<# ( -- )`

- Prepare pictured number output buffer.

`u. ( u -- )`

- Print unsigned single number.

`. ( n -- )`

- Print single number.

`ud. ( ud -- )`

- Print unsigned double number.

`d. ( d -- )`

- Print double number.

`hold< ( char -- )`

- Adds character to pictured number output buffer from behind.

`f#S ( n-comma1 -- n-comma2 )`

- Adds 32 comma-digits to number output.

`f# ( n-comma1 -- n-comma2 )`

- Adds one comma-digit to number output.

`f. ( df -- )`

- Prints a fixed-point number with 32 fractional digits.

`f.n ( df n -- )`

- Prints a fixed-point number with n fractional digits.

`hex. ( u -- )`

- Prints 32 bit unsigned in hex base, needs emit only. This is independent of number subsystem.

## jkotlinski Printf-Style Printing and Formatting

[printf.fs](../../../../fs/forth/printf.fs)

`sprintf ( n*x addr1 u1 addr2 -- addr2 u3 )`

- Prints n*x into buffer addr2 using the format string at addr1 u. addr2 u3 is the resulting string.

`printf ( n*x c-addr u -- )`

- Prints n*x using the format string at caddr u. The format string consists of ordinary characters (except %), which are copied verbatim to the destination buffer, and conversion specifications. Conversion specifications have the following structure:

- Introductory % character
- An optional '-' that signifies left justification
- An optional '0' that pads with zeros instead of spaces
- An optional decimal integer value specifying the minimum field width
- A conversion format specifier

Format Specifiers:

```
% - %
c - character
n - signed number
u - unsigned number
x - hexadecimal number
dn - double-cell signed number
du - double-cell unsigned number
s - string (c-addr u)
```

Examples:
```
10 s" Joe" s" %s has a %n%% discount!" printf
Joe has a 10% discount! ok

10 s" %05n" printf
00010 ok

s" spaced" s" %-10s out" printf
spaced     out ok``
```

## Deep insights

[deepinsight.s](../../../../sw/components/forth/deepinsight.s)

`words ( -- )`

- Prints list of defined words.

`.s ( many -- many )`

- Prints stack contents, signed.

`u.s ( many -- many )`

- Prints stack contents, unsigned.

`h.s ( many -- many )`

- Prints stack contents, unsigned, hex.

`.rs ( many -- many )`

- Prints return stack contents.

`unused ( -- u )`

- Get current amount of free memory.

## User input and its interpretation (exactly ANS, some logical extensions)

[query.s](../../../../sw/components/forth/query.s)
[token.s](../../../../sw/components/forth/token.s)
[interpreter.s](../../../../sw/components/forth/interpreter.s)

See [Interpreting Console Input](interpreting.md).

`query ( -- )`

- Fetches user input to input buffer.

`tib ( -- cstr-addr )`

- Input buffer.

`current-source ( -- addr )`

- Double variable which contains source.

`setsource ( c-addr len -- )`

- Change source.

`source ( -- c-addr len )`

- Current source.

`>in ( -- addr )`

- Variable with current offset into source.

`accept ( c-addr maxlength -- length )`

- Read input into a string.

`token ( -- c-addr len )`

- Cuts one token out of input buffer.

`parse ( char -- c-addr len )`

- Cuts anything delimited by char out of input buffer.

`evaluate ( any addr len -- any )`

- Interpret given string.

`interpret ( any -- any )`

- Execute, compile, fold, optimize...

`quit ( many -- ) (R: many -- )`

- Resets Stacks.

`hook-quit ( -- a-addr )`

- Hook for changing the inner quit loop.

`(quit) ( any -- any )`

- Standard REPL, default for hook-quit.

## Dictionary expansion

[compiler.s](../../../../sw/components/forth/compiler.s)
[compiler-memory.s](../../../../sw/components/forth/compiler-memory.s)
[calculations.s](../../../../sw/components/forth/calculations.s)
[utils.fs](../../../../fs/forth/utils.fs)

`alignto ( a power -- a )`

- Align an address to a power of two.

`align ( -- )`

- Word-align dictionary pointer.

`halign ( -- )`

- Halfword-align dictionary pointer.

`aligned ( c-addr -- a-addr )`

- Advances to next aligned address.

`haligned ( c-addr -- a-addr )`

- Advances to next half-word aligned address.

`cell+ ( x -- x+4 )`

- Add size of one cell.

`cells ( n -- 4*n )`

- Calculate size of n cells.

`allot ( n -- )`

- Tries to advance Dictionary Pointer by n bytes. Aborts, if not enough space available.

`here ( -- a-addr|h-addr )`

- Gives current position in Dictionary.

`(dp) ( -- a-addr )`

- Variable: Dictionary pointer.

`(latest) ( -- a-addr )`

- Variable: Latest definition.

`, ( u|n -- )`

- Appends a single number to dictionary.

`h, ( u|n -- )`

- Appends a halfword to dictionary.

`c, ( u|n -- )`

- Appends a byte to dictionary.

`forget ( "word" -- )`

- Forget the given word.

` del ( -- )`

- Delete the latest definition.

`forgetall ( -- )`

- Forget everything except the Forth Core.

`compileto>` ( -- f )

- Retrieve compile-to state.

`>compileto` ( f -- )

- Restore compile-to state.

`compiletoemem? ( -- ? )`

- Currently compiling into emem?

`compiletoemem ( -- )`

- Makes emem the target for compiling.

`compiletoimem ( -- )`

- Makes imem the target for compiling.

`addrinimem? ( addr -- flag )`

- Location in imem?

`addrinemem? ( addr -- flag )`

- Location in emem?

`string, ( c-addr len -- )`

- Inserts a string of maximum 255 characters without runtime.

`literal, ( u|n -- )`

- Compiles a literal with runtime.

`inline, ( a-addr -- )`

- Inlines the chosen subroutine.

`call, ( a-addr -- )`

- Compiles a call to a subroutine.

`ret, ( -- )`

- Compiles a ret opcode.

`ramvar-here ( -- a-addr )`

- Gives current RAM management pointer.

`dictionarystart ( -- a-addr )`

- Current entry point for dictionary search.

`dictionarynext ( a-addr -- a-addr flag )`

- Scans dictionary chain and returns true if end is reached.

`skipdefinition ( addr -- addr* )`

- Skip after the next ret opcode.

`(sp) ( -- a-addr)`

- Variable to compare data stack pointer before and after compilation of definitions.

`registerliteral, ( x register -- )`

- Generate shortest possible sequence to get x into given register.

## Flags and inventory

Note that `[immediate]` needs to be *inside* of the definition, not after the `;`. There is no `immediate` Word variant that goes *after* the definition.

[compiler-memory.s](../../../../sw/components/forth/compiler-memory.s)
[compiler.s](../../../../sw/components/forth/compiler.s)

`smudge ( -- )`

- Makes current definition visible, takes care of proper ending.

`[inline] ( -- )`

- Makes current definition inlineable.

`[noframe] ( -- )`

- No need to push/pop link register when compiling this definition.

`[immediate] ( -- )`

- Makes current definition immediate.

`[compileonly] ( -- )`

- Makes current definition compile-only.

`setflags ( x -- )`

- Sets Flags with a mask. This isn't immediate, but for flash, place it inside your definition!

`(create) name   ( -- )`

- Creates and links a new invisible dictionary header that does nothing.

`find ( c-addr len -- a-addr flags )`

- Searches for a String in Dictionary. Gives back flags, which are different from ANS!

`hook-find ( -- a-addr )`

- Hook for redirecting find.

`(find) ( c-addr len -- a-addr flags )`

- Default find implementation.

`[0-foldable] ( -- )`

- Current word becomes foldable with zero constants.

`[1-foldable] ( -- )`

- Current word becomes foldable with one constant.

`[2-foldable] ( -- )`

- Current word becomes foldable with two constants.

...

`[7-foldable] ( -- )`

- Current word becomes foldable with 7 constants.

## Compiler essentials

[compiler.s](../../../../sw/components/forth/compiler.s)
[buildsdoes.s](../../../../sw/components/forth/buildsdoes.s)

`execute ( xt -- )`

- Calls the executable token.

`recurse ( -- )`

- Enables the current definition to call itself.

`' ( "Word" -- addr )`

- Tries to find Word in dictionary and put its address on the data stack.
Quits (i.e., restarts REPL) if Word is not found.

`['] (compile-time: "Word" -- ) (run-time: -- addr )`

- Compile-only Immediate. Compiles the executable address of found Word as literal.

`postpone ( "Word" -- )`

- Used inside Immediate Words. Selects the compile-time behavior of the postponed Word.

Example:
```
\ Begin lambda
: [: ( -- )
  state @ if
    \ [: is invoked as a compiling word, i.e. a code-generating word that
    \ executes when it's encountered in the definition of another word.
    \ When [: _executes_... it compiles an 'ahead'. This ahead pushes
    \ 2 items on the stack: patchaddr and structmatchconst
    postpone ahead ( patchaddr structmatchconst )
    \ [: puts 'here' on the stack. This is the entry point of the code that 'ahead' is skipping
    \ over.
    here -rot ( lambdaentry patchaddr structmatchconst )
    \ [: compiles an 'add sp, sp -4 sw ra, (sp)', i.e. it generates a prologue.
    postpone push_ra ( lambdaentry patchaddr structmatchconst )
  else
    \ [: is invoked while in execution state.
    ] \ Enter compilation state and push following 3 items on the stack for ;] to consume.
    here 0 0 \ ( lambdaentry patchaddr structmatchconst )
    postpone push_ra
  then
  [immediate]
;
```

`create ( "name" -- )`

- Takes a word from the input stream and creates a dictionary entry with that name.

`: <Defining Word> create <compile-time operations> does> <run-time operations> ;`

- `does> ( Compile-time: -- )` marks the end of the compile-time behavior and the beginning of the run-time
behavior of the Defining Word.

- `does> ( Run-time: -- addr )` puts the address of the defined Word instance on the data stack.

Example:

```
: constant create , does> @ ;

42 constant istheanswer

istheanswer . cr
```

`state ( -- a-addr )`

- Address of state variable.

`] ( -- )`

- Switch to compile state.

`[ ( -- )`

- Switch to execute state.

`; ( -- )`

- Finishes new definition.

`: name          ( -- )`

- Opens new definition.

## Control structures

Control structures are immediate and compile-only.

Internally they have complicated compile-time stack effects.

### Decisions

[controlstructures.s](../../../../sw/components/forth/controlstructures.s)

```
flag if ... then
flag if ... else ... then
```

- This is the common `flag if ... [else ...]` then structure.

```
ahead ... then
```

- Unconditionally jump over `...` to the statement after `then`.

Case:

[case.s](../../../../sw/components/forth/case.s)

```
n case
     m1   of ... endof
     m2   .. ... .....
   flag  ?of ... endof
    all others
  endcase
```

`case ( n -- n )`

- Begins case structure.

`of ( m -- )`

- Compares m with n, choose this if n=m.

`?of ( n flag -- )`

- Flag-of, for custom comparisons.

`endof ( -- )`

- End of one possibility.

`endcase ( n -- )`

- Ends case structure, discards n.

### Indefinite Loops

[controlstructures.s](../../../../sw/components/forth/controlstructures.s)

```
begin ... again
begin ... flag until
begin ... flag while ... repeat
begin ... flag while ... flag while ... repeat ... else ... then
```

`repeat ( -- )`

- Finish of a middle-flag-checking loop.

`while ( flag -- )`

- Check a flag in the middle of a loop.

`until ( flag -- )`

- begin ... flag until.

`again ( -- )`

- begin ... again is an endless loop.

`begin ( -- )`


-
### Definite Loops

[doloop.s](../../../../sw/components/forth/doloop.s)
[compiler.s](../../../../sw/components/forth/compiler.s)

```
limit index   do ... [one or more leave(s)] ... loop
             ?do ... [one or more leave(s)] ... loop
              do ... [one or more leave(s)] ... n +loop
             ?do ... [one or more leave(s)] ... n +loop
```

`k ( -- u|n )`

- Gives third  loop index.

`j ( -- u|n )`

- Gives second loop index.

`i ( -- u|n )`

- Gives innermost loop index.

`unloop (R: old-limit old-index -- )`

- Drops innermost loop structure, pops back old loop structures to loop registers.

`exit ( -- )`

- Returns from current definition. Compiles a ret opcode.

`leave ( -- ) (R: old-limit old-index -- )`

- Leaves current innermost loop promptly.

`+loop ( u|n -- ) (R: unchanged | old-limit old-index -- )`

- Adds number to current loop index register and checks whether to continue or not.

`loop ( -- ) (R: unchanged | old-limit old-index -- )`

- Increments current loop index register by one and checks whether to continue or not.

`?do ( Limit Index -- ) (R: unchanged | -- old-limit old-index )`

- Begins a loop if limit and index are not equal.

`do ( Limit Index -- ) (R: -- old-limit old-index )`

- Begins a loop.

## C Foreign Function Interface

[early.fs](../../../../fs/forth/early.fs)
[cstr.fs](../../../../fs/forth/cstr.fs)

`c-fun Define: ( compile time: fun "name" -- ) ( run time: i*x -- j*x )`

- Register a C function so it can be called from Forth. See [Forth Calling C](c-ffi.md#forth-calling-c).

`CSTRLENMAX`

- Maximum C string length constant.

`s0len ( addr -- u )`

- calculate the length of a 0-terminated string.

`s0>s ( adr -- adr len)`

- Convert 0-terminated string to Forth string.

`+0c! ( adr len -- )`

- 0-terminate Forth string, assume there's space for the 0-terminator.

`s0" ( "string" -- adr )`

- Compiles a 0-terminated string and gives back its address when executed. May raise `x-string-too-long`.

`cstr ( buf256 adr len -- )`

- Copy len bytes from addr to buf256 of at least 256 bytes and add 0-terminator. May raise `x-string-too-long`.

`cstr" ( buf256 "text" -- )`

- Copy Forth string into buffer of at least 256 bytes and 0-terminate. May raise `x-string-too-long`.

Exceptions:
```
x-string-too-long
```

## Filesystem

[fs.fs](../../../../fs/forth/fs.fs)

File Access:

`f_open ( addr len mode -- fil )`

- Open the file specified in the input string. The mode argument is a combination of the following values:

    - `FA_READ`: Specifies read access to the file. Data can be read from the file.
    - `FA_WRITE`: Specifies write access to the file. Data can be written to the file. Combine with FA_READ for read-write access.
    - `FA_OPEN_EXISTING`: Opens the existing file. The function fails if the file does not exist (default).
    - `FA_CREATE_ALWAYS`: Creates a new file and opens it. If the file already exists, it is truncated and overwritten.
    - `FA_CREATE_NEW`: Creates a new file. The function fails if the file already exists.
    - `FA_OPEN_ALWAYS`: Opens the file or creates a new one if it does not exist.
    - `FA_OPEN_APPEND`: Opens the existing file and sets the read/write pointer to the end of the file.

  May throw `x-fr-*` and `x-pool-*` exceptions.

`f_close ( fil -- )`

- Close file

`f_read ( fil buf numbytes -- numbytes )`

- Read n bytes from file into buffer
  May throw `x-fr-*` exception.
  May throw `x-fr-*` exception.

`f_write ( fil buf numbytes -- numbytes )`

- Write n bytes from buffer into file. May throw `x-fr-*` exception.

`f_lseek ( fil offset -- )`

- Moves the file read/write pointer of an open file. Can also be used to expand
  the file size (cluster pre-allocation).
  May throw `x-fr-*` exception.

`f_truncate ( fil -- )`

- Truncate file size to current read/write pointer. May throw `x-fr-*` exception.

`f_sync ( fil -- )`

- Flush cached data May throw `x-fr-*` exception.

`f_gets ( fil buf buflen -- adr len )`

- Read a string from the file.
  Note that reading from eof returns 0 0 as adr len.
  Note that when eof or buflen is reached,
       the returned line might not contain a \n.
  May throw `x-fr-*` exception.

`f_putc ( fil c -- )`

- Write a character to the file May throw `x-fr-*` exception.

`f_tell ( fil -- rwptr )`

- Get current read/write pointer.

`f_eof ( fil -- flag )`

- Test for end of file.

`f_size ( fil -- size )`

- Get file size.

`f_error ( fil -- flag )`

- Test for an error.

Directory Access:

`f_opendir ( addr len -- dir )`

- Open the directory specified in the input string. May throw `x-fr-*` and `x-pool-*` exceptions.

`f_closedir ( dir -- )`

- Close directory.

`f_readdir ( dir -- )`

- Read directory entry into the filinfo object. May throw `x-fr-*` exception. May throw `x-fr-*` exception.

`f_findfirst ( addr1 len1 addr2 len2 -- dir )`

- Open the directory specified in addr/len1 input string and read first
  item matching pattern specified in addr/len2 input string.
  Put result in filinfo object.
  May throw `x-fr-*` exception.
  See pattern-each for an easy-to-use wrapper around f_findfirst/next.

`f_findnext ( dir -- )`

- Find next entry matching pattern.
  Put result in filinfo object.
  May throw `x-fr-*` exception.

File and Directory Management:

`f_stat ( addr len -- )`

- Check existence of file or directory with given name. Put result in filinfo.
  May throw `x-fr-*` exception.

`f_unlink ( addr len -- )`

- Remove file or subdirectory with given name. May throw `x-fr-*` exception.

`f_rename ( addr1 len1 addr2 len2 -- )`

- Rename/move fil/dir in addr/len1 string addr/len2 string. May throw `x-fr-*` exception.

`f_chmod ( addr len attr mask -- )`

- Change attribute of file or directory with given name.
  Attribute values: `AM_RDO`, `AM_ARC`, `AM_SYS`, `AM_HID`
  May throw `x-fr-*` exception.

`f_utime ( addr len -- )`

- Change timestamp (time and date) of file or subdirectory with given name to value specified in filinfo.
  May throw `x-fr-*` exception.

`filinfo` is a global object, populated by directory operations. The following Words are the object's accessors:

- `filinfo.fsize ( -- fsize )` Retrieve file size from global filinfo object.

- `filinfo.setfdate ( dd mm yyyy -- )` Set date in global filinfo object.

- `filinfo.getfdate ( -- dd mm yyyy )` Retrieve date from global  filinfo object.

- `filinfo.setftime ( hh mm ss -- )` Set time in global filinfo object.

- `filinfo.getftime ( -- hh mm ss )` Retrieve time from global filinfo object.

- `filinfo.fattrib ( -- attrib )` Retrieve attributes from global filinfo object: `AM_RDO, AM_ARC, AM_SYS, AM_HID, AM_DIR.`

- `filinfo.fname ( -- c-addr len )` Retrieve name from global filinfo object.

`f_mkdir ( addr len -- )`

- Make directory with given name. May throw `x-fr-*` exception.

`f_chdir ( addr len -- )`

- Change current directory to directory with given name. May throw `x-fr-*` exception.

`f_getcwd ( -- addr len )`

- Return current working directory name.
  The given string object remains valid until the next `f_*` operation.
  May throw `x-fr-*` exception.

`f_cmp ( addr0 len0 addr1 len1 -- f )`

- Compare two files, identified by filename. Pushes true if identical, false otherwise.

`glob-each ( pata patl xt -- )`

- Iterate over each file matching given glob pattern string, invoke xt passing in full file path.

Example:

```
: rm
  cr
  token
  [: ( patha pathl )
    2dup s" Removing: %s" printf cr ( patha pathl )
    f_unlink ( dir )
  ;] ( pata patl xt )
  glob-each
;
```

Volume Management and System Configuration:

`f_mount ( addr len -- )`

- Mount a volume. Pass in volume name, e.g. `s" ram:"`, or `s" sd0:"`. May throw `x-fr-*` exception.

`f_umount ( addr len -- )`

- Unmount a volume. Pass in volume name, e.g. `s" ram:"`, or `s" sd0:"`. May throw `x-fr-*` exception.

`f_chdrive ( addr len -- )`

- Change drive. Pass in volume name, e.g. `s" ram:"`, or `s" sd0:"`. May throw `x-fr-*` exception.

`f_getfree ( addr len -- tot free )`

- Get total and free bytes on volume.
  Pass in volume name, e.g. `s" /ram"`, or `s" /sd"`
  May throw `x-fr-*` exception.

Pathname accessors:

`dirname ( addr len -- addr len )`

- Extract the directory name portion of a path string:

`basename ( addr len -- addr len )`

- Extract the basename portion of a path string:

`pathname ( dira dirl basa basl buf -- patha pathl )`

- Combine a directory name and a basename into a full path name.

`ior>exception ( ior -- xt)`

- FATFS IO result (`FRESULT`) to exception mapping.

`check-throw-ior ( ior -- )`

- Check FatFS IO result and throw exception if non-zero.

`.ior ( ior -- )`

- Print the IO return code string.

Exceptions:
```
x-fr-disk-err
x-fr-int-err
x-fr-not-ready
x-fr-no-file
x-fr-no-path
x-fr-invalid-name
x-fr-denied
x-fr-exist
x-fr-invalid-object
x-fr-write-protected
x-fr-invalid-drive
x-fr-not-enabled
x-fr-no-filesystem
x-fr-mkfs-aborted
x-fr-timeout
x-fr-locked
x-fr-not-enough-core
x-fr-too-many-open-files
x-fr-invalid-parameter
x-fr-unknown
```

## Output Redirection to File

[fs-redirect.fs](../../../../fs/forth/fs-redirect.fs)

`emit>file ( fil -- )`

- Set emit-hook and emit-hook? to emit-to-open-file handlers.

`emit>console ( -- )`

- Set emit-hook and emit-hook? to console handlers.

`emit>null ( -- )`

- Set emit-hook and emit-hook? to drop all output.

`tee>file ( fil -- )`

- Set emit-hook and emit-hook? to emit-to-open-file-and-console handlers.
  Install emit hook forwarding to open file with given description,
  then invoke previous emit handler.

`tee-end ( -- )`

- Remove `tee>file` emit hook. Restore previous emit handler.

# Shell

[shell.fs](../../../../fs/forth/shell.fs)

Some basic shell-like commands for interactive use.

`ls ( "glob-pattern" -- )`

- ls with glob pattern string: `ls ./*.fs` , `ls ../*`.

`mkdir ( "dirname" -- )`

- Make directory. `mkdir <dirname>`.

`cd ( "dirname" -- )`

- Change directory. `cd <dirname>`.

`pwd ( -- )`

- Print working directory.

`chmod ( "glob-pattern" "+/-attrib" -- )`

- `chmod <fname> +/-<attrib>`. e.g. `chmod dir/file.* -rdo`.

`mv ( "from" "to" -- )`

- `mv <source-file> <dest-file>`
- `mv <source-file> <dest-dir>`
- `mv <source-pattern> <dest-dir>`
- `*` and `?` are wildcards in the pattern string.

`cp ( "from" "to" -- )`

- `cp <source-file> <dest-file>`
- `cp <source-file> <dest-dir>`
- `cp <source-pattern> <dest-dir>`
- `*` and `?` are wildcards in the pattern string.

`cat ( "filename" -- )`

- `cat <filename>`.

`rm ( "pattern" -- )`

- `rm <file or dir pattern string>`, e.g. `rm dir/*.txt`
  `\` `*` and `?` are wildcards in the pattern string.

`chdrv ( "volume" -- )`

- Change drive. Supported volumes are `sd0:` and `ram:`

`umount ( "volume" -- )`

- Unmount a volume. Supported volumes are `sd0:` and `ram:`

`mount ( "volume" -- )`

- Mount a volume. Supported volumes are `sd0:` and `ram0:`

`df ( "volume" -- )`

- Get volume usage info. Supported volume names are `sd0:` and `ram0:`

`>null ( xt -- )`

- Execute xt with all output suspended.
  Usage: `[: <statements to be executed with output suspended :] >null`

`>file ( xt "filename" -- )`

- Redirect stdout to given file in overwrite mode. Don't emit to console.
  Usage: `[: <statements to be executed with redirection ;] >file file.log`

`&>file ( xt "filename -- )`

- Redirect stdout to given file in overwrite mode and emit to console.
  Usage: `[: <statements to be executed with redirection ;] tee>file file.log`

`>>file ( xt "filename" -- )`

- Redirect stdout to given file in append mode. Don't emit to console.
  Usage: `[: <statements to be executed with redirection ;] >>file file.log`

`include ( any "path" -- any )`

- Load and evaluate forth code from a file with the given path.
  `include` may be used recursively, i.e., an included file
  may contain one or more include calls.
  May raise `x-line-truncated`.

- See [Include File Evaluation](include.md)

`refill ( -- )`

- An alternative query that also supports input
  from file. The file id is indicated in variable
  `include-source-id`. If set to 0, `refill` invokes
  `query`.
  May raise `x-line-truncated` and `x-eof`.

Exceptions:
```
x-eof
x-line-truncated
```

## Conditional Execution/Compilation

[ifdef.fs](../../../../fs/forth/ifdef.fs)

`[if] (? -- )`

- Start conditional execution/compilation if flag is true.

`[ifdef] ( "Word" -- )`

- Start conditional execution/compilation if Word is defined.

`[ifndef] ( "Word" -- )`

- Start conditional execution/compilation if Word is not defined.

`[else] ( -- )`

- Start the else-case in `[if(n)(def)]/[else]/[then]`.

`[then] ( -- )`

- Finish conditional execution/compilation.

Example from [init.fs](../../../../fs/forth/init.fs):
```
include /forth/ifdef.fs

include /forth/disasm.fs
include /forth/dump.fs
include /forth/dict.fs

\ This flag is set when building the boxkerntest target.
[ifdef] FORTH_CORE_TEST
false include-verbose !
include /test/testsuite.fs
[then]

```

## Mecrisp RISC-V 32 IM Disassembler

[disasm.fs](../../../../fs/forth/disasm.fs)
[dump.fs](../../../../fs/forth/dump.fs)

`disasm-$`

- Variable holding current position for disassembling.

`disasm-step ( -- )`

- Disassemble instruction at `disasm-$` and advance `disasm-$`.

`list ( -- )`

- A quick list of Words.

`see ( "Word" -- )`

- Disassemble the definition of Word from beginning to first ret.

`dump ( addr len -- )`

- Print the contents of a memory region.

## Interrupt Words

See also [Interrupt Handling](irqs.md).

`reset ( -- )`

- Reset on hardware level, if possible.

`dint ( -- )`

- Disables interrupts.

`eint ( -- )`

- Enables interrupts.

`eint ( -- )`

- Are interrupts enabled ?

`wfi ( -- )`

- Wait For Interrupt.

`nop ( -- )`

- No Operation. Hook for unused handlers.

`unhandled ( -- )`

- Message for unhandled interrupts.

`fault ( -- a-addr )`

- For all faults, if available.

`mcause ( -- mcause)`

- Which interrupt?

`mepc ( -- mepc )`

- Where did it occur?

`mie ( -- mie )`

- Get Machine Interrupt Enable.

`mip ( -- mip )`

- Get Machine Interrupt Pending.

`mie ( mie -- )`

- Set Machine Interrupt Enable.

`mip ( mip -- )`

- Set Machine Interrupt Pending.

`dfx ( -- a-addr )`

- DFX irq hook.

`uart ( -- a-addr )`

- uart irq hook.

`i2c ( -- a-addr )`

- i2c irq hook.

`usb_hid_0 ( -- a-addr )`

- usb_hid_0 irq hook.

`usb_hid_1 ( -- a-addr )`

- usb_hid_1 irq hook.

`gpio ( -- a-addr )`

- gpio irq hook.

`sdspi ( -- a-addr )`

- sdspi irq hook.

`vs0 ( -- a-addr )`

- vs0 irq hook.

`vera ( -- a-addr )`

- vera irq hook.

`timer ( -- a-addr )`

- timer irq hook.

`irq ( -- id )`

- VERA irq id.

`irq ( -- id )`

- VS0 irq id.

`irq ( -- id )`

- DFX irq id.

`irq ( -- id )`

- SDSPI irq id.

`irq ( -- id )`

- USB-HID-1 irq id.

`irq ( -- id )`

- USB-HID-0 irq id.

`irq ( -- id )`

- I2C irq id.

`irq ( -- id )`

- UART irq id.

Example:
```
\ IRQ handler routine
: mtime-irq-handle
  ." IRQ mip : " mip hex. cr
  ." IRQ mcause : " mcause hex. cr
  -1 s>d mtimecmp64! \ Disable timer IRQ
  ." MTIMER IRQ received." cr ;

dint
0 mie!
\ Install timer interrupt handler hook
' mtime-irq-handle irq-timer !
\ Disable timer

-1 s>d mtimecmp64!
\ Enable timer IRQ
1 irq-id-timer lshift mie! eint
\ Request timer IRQ 3 seconds from now.
50000000 3 * set-raw-time-cmp
```

## Time Handling Words

`mtime64 ( -- ud )`

- Get 64-bit mtime.

`mtimecmp64 ( -- ud )`

- Get 64-bit mtime comparator.

`mtimecmp64 ( ud -- )`

- Set 64-bit mtime comparator.

`set ( offset - )`

- Set mtime comparator using an offset relative to current time.

`cycles64 ( -- ud )`

- Get 64-bit uptime in cycles.

Example: Interrupt Handling Example in previous subsection.

