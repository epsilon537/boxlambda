# Forth Word Glossary

I decided to put all Forth Words on a single page for easy reference. Some Words will require more context than a one-liner can describe.
In such case, the one-liner contains a link to a page with additional info.

Most of these Words are not created by me. They come from Mecrisp Forth, ZeptoForth and other resource. As much as I would like to give
credit where credit is due, I don't think it's particularly helpful to do it on this already jam-packed page.
Where applicable, the Forth source code contains references to their origins.

## Constants and Units

[units.fs](../../../fs/units.fs)

`cell` The size in bytes of one cell

`max-uint` Maximum unsigned integer value

`max-int` Maximum signed integer value

`min-int` Minimum signed integer value

`cells+ ( x n -- x+n*cell )` Add the size of n cells to x

`chars ( u -- u )`
- Returns the size of u chars (which is u). Used for clarity, to put a unit behind a number. e.g. `8 chars`

`char ( a -- a+1 )` Advance a by one char.

## Range Related Words

`span ( addr len -- start end )` Convert addr len to start-end address span.

`bounds ( addr len -- end start )` Convert addr len to end-start address span.

`within ( n low high -- flag )` True if n is within the range [low..high[

## Arrays

`array <name> ( n -- ) executes: ( i -- addr)`
  - Create an array of n cells with given name. `i <name>` returns the address of the i-th cell.

`carray <name> ( n -- ) executes: ( i -- addr)`
  - Create an array of n bytes with given name. `i <name>` returns the address of the i-th byte.

## Terminal-IO  (exactly ANS, some logical extensions)

[terminal.s](../../../sw/components/forth/terminal.s)words.mdwords.md
[terminalhooks.s](../../../sw/components/forth/terminalhook.s)

`emit? ( -- Flag )` Ready to send a character ?

`key? ( -- Flag )` Checks if a key is waiting

`key ( -- Char )` Waits for and fetches the pressed key

`emit ( Char -- )` Emits a character.

`hook-emit? ( -- a-addr )` Hooks for redirecting

`hook-key? ( -- a-addr )` terminal IO

`hook-key ( -- a-addr )` on the fly

`hook-emit ( -- a-addr )`

`serial-emit? ( -- Flag )` Serial interface

`serial-key? ( -- Flag )` terminal routines

`serial-key ( -- Char )` as default communications

`serial-emit ( Char -- )`

`hook-pause ( -- a-addr )` Hook for a multitasker

`pause ( -- )` Task switch, none for default

## Stack Jugglers  (exactly ANS, some logical extensions)

[stackjugglers.s](../../../sw/components/forth/stackjugglers.s)
[utils.fs](../../../fs/forth/utils.fs)

### Single-Jugglers

`depth ( -- +n )` Gives number of single-cell stack items.

`nip ( x1 x2 -- x2 )`

`drop ( x -- )`

`rot ( x1 x2 x3 -- x2 x3 x1 )`

`-rot ( x1 x2 x3 -- x3 x1 x2 )`

`swap ( x1 x2 -- x2 x1 )`

`tuck ( x1 x2 -- x2 x1 x2 )`

`over ( x1 x2 -- x1 x2 x1 )`

`?dup ( x -- 0 | x x )`

`dup ( x -- x x )`

`pick ( ... xi+1 xi ... x1 x0 i -- ... x1 x0 xi )` Picks one element from deep below

`roll ( xu ... x0 u -- xu-1 ... x0 xu )`
- Roll takes the element u deep in the stack and moves it to the top, shifting the elements above it down by one.

`>r ( x -- ) (R: -- x )`

`r> ( -- x ) (R: x -- )`

`r@ ( -- x ) (R: x -- x )`

`rdrop (  --  ) (R: x -- )`

`rdepth ( -- +n ) Gives number of return stack items.`

`rpick ( i -- xi ) R: ( ... xi ... x0 -- ... xi ... x0 )`

### Double-Jugglers

[double.s](../../../sw/components/forth/double.s)

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

[stackjugglers.s](../../../sw/components/forth/stackjugglers.s)

`sp@ ( -- a-addr )` Fetch  data stack pointer

`sp! ( a-addr -- )` Store  data stack pointer

`rp@ ( -- a-addr )` Fetch return stack pointer

`rp! ( a-addr -- )` Store return stack pointer

## Logic and Bit Manipulation

[logic.s](../../../sw/components/forth/logic.s)
[utils.fs](../../../fs/forth/utils.fs)

Shifts decode the lowest 5 bits only on RISC-V. Therefore, ar/r/lshift behaves
like "31 and ar/r/lshift". 32 lshift does nothing.

`arshift ( x1 u -- x2 )` Arithmetic right-shift of u bit-places

`rshift ( x1 u -- x2 )` Logical right-shift of u bit-places

`lshift ( x1 u -- x2 )` Logical  left-shift of u bit-places

`shr ( x1 -- x2 )` Logical right-shift of one bit-place

`shl ( x1 -- x2 )` Logical  left-shift of one bit-place

`ror ( x1 -- x2 )` Logical right-rotation of one bit-place

`rol ( x1 -- x2 )` Logical  left-rotation of one bit-place

`bitval ( u -- u' )` Integer value corresponding to bit position u (i.e. 1<<u).

`bic ( x1 x2 -- x3 )` Bit clear, identical to "not and"

`not ( x1 -- x2 )` Invert all bits

`xor ( x1 x2 -- x3 )` Bitwise Exclusive-OR

`or ( x1 x2 -- x3 )` Bitwise OR

`and ( x1 x2 -- x3 )` Bitwise AND

`false ( --  0 )` False-Flag

`true ( -- -1 )` True-Flag

`clz ( x1 -- u )` Count leading zeros

## Calculus

### Single Number Calculus (exactly ANS, some logical extensions)

[multiplydivide.s](../../../sw/components/forth/multiplydivide.s)
[calculations.s](../../../sw/components/forth/calculations.s)

`u/mod ( u1 u2 -- u3 u4 )` 32/32 = 32 rem 32 Division
                                           u1 / u2 = u4 remainder u3
`/mod ( n1 n2 -- n3 n4 )` n1 / n2 = n4 rem n3

`mod ( n1 n2 -- n3 )` n1 / n2 = remainder n3

`/ ( n1 n2 -- n3 )` n1 / n2 = n3

`* ( u1|n1 u2|n2 -- u3|n3 )` 32*32 = 32 Multiplication

`2- ( u1|n1 -- u2|n2 )` Subtracts two, optimized

`1- ( u1|n1 -- u2|n2 )` Subtracts one, optimized

`2+ ( u1|n1 -- u2|n2 )` Adds two, optimized

`1+ ( u1|n1 -- u2|n2 )` Adds one, optimized

`even ( u1|n1 -- u2|n2 )` Makes even. Adds one if uneven.

`2* ( n1 -- n2 )` Arithmetic  left-shift

`2/ ( n1 -- n2 )` Arithmetic right-shift

`abs ( n -- u )` Absolute value

`negate ( n1 -- n2 )` Negate

`- ( u1|n1 u2|n2 -- u3|n3 )` Subtraction

`+ ( u1|n1 u2|n2 -- u3|n3 )` Addition

### Double Number Calculus (exactly ANS, some logical extensions)

[double.s](../../../sw/components/forth/double.s)
[multiplydivide.s](../../../sw/components/forth/multiplydivide.s)
[utils.fs](../../../forth/utils.fs)

`um+ ( u1 u2 -- u carry )` Unsigned addition with carry.

`um* ( u1 u2 -- ud )` 32*32 = 64 Multiplication

`ud* ( ud1|d1 ud2|d2 -- ud3|d3 )` 64*64 = 64 Multiplication

`udm* ( ud1 ud2 -- ud3-Low ud4-High )` 64*64=128 Multiplication

`um/mod ( ud u1 -- u2 u3 )` ud / u1 = u3 remainder u2

`ud/mod ( ud1 ud2 -- ud3 ud4 )` 64/64 = 64 rem 64 Division ud1 / ud2 = ud4 remainder ud3

`m+ ( d n -- d' )` Add n to d

`m* ( n1 n2 -- d )` n1 * n2 = d

`m/mod ( d  n1 -- n2 n3 )` d  / n1 = n3 remainder r2

`d/mod ( d1 d2 -- d3 d4 )` d1 / d2 = d4 remainder d3

`d/ ( d1 d2 -- d3 )` d1 / d2 = d3

`*/ ( n1 n2 n3 -- n4 )` n1 * n2 / n3 = n4

`u*/ ( u1 u2 u3 -- u4 )` u1 * u2 / u3 = u4

`*/mod ( n1 n2 n3 -- n4 n5 )` n1 * n2 / n3 = n5 remainder n4

`u*/mod ( u1 u2 u3 -- u4 u5 )` u1 * u2 / u3 = u5 remainder u4

`d2* ( d1 -- d2 )` Arithmetic  left-shift

`d2/ ( d1 -- d2 )` Arithmetic right-shift

`dshl ( ud1 -- ud2 )` Logical  left-shift, same as d2*

`dshr ( ud1 -- ud2 )` Logical right-shift

`dabs ( d -- ud )` Absolute value

`dnegate ( d1 -- d2 )` Negate

`d- ( ud1|d1 ud2|d2 -- ud3|d3 )` Subtraction

`d+ ( ud1|d1 ud2|d2 -- ud3|d3 )` Addition

`s>d ( n -- d )` Makes a signed single number double length

`2arshift ( d1 u -- d2 )` Arithmetic double right-shift of u bit-places

`2rshift ( d1 u -- d2 )` Logical    double right-shift of u bit-places

`2lshift ( d1 u -- d2 )` Logical    double  left-shift of u bit-places

## Fixed point numbers

S31.32 fixpoint numbers are written like `3,14159`, i.e. written with a comma
instead of a dot.

Fixpoint numbers are stored ( n-comma n-whole ) and can be handled
like signed double numbers.

[double.s](../../../sw/components/forth/double.s)

`f/ ( df1 df2 -- df3 )` Division of two fixpoint numbers

`f* ( df1 df2 -- df3 )` Multiplication

## Comparisons  (exactly ANS, some logical extensions)

### Single Comparisons

[comparisons.s](../../../sw/components/forth/comparisons.s)
[utils.fs](../../../fs/forth/utils.fs)

`u<= ( u1 u2 -- flag )` Unsigned comparisions

`u>= ( u1 u2 -- flag )`

`u> ( u1 u2 -- flag )`

`u< ( u1 u2 -- flag )`

`<= ( n1 n2 -- flag )` Signed comparisions

`>= ( n1 n2 -- flag )`

`> ( n1 n2 -- flag )`

`< ( n1 n2 -- flag )`

`0< ( n - flag )` Negative ?
`0> ( n - flag )` Positive ?

`0<> ( x -- flag )`

`0= ( x -- flag )`

`<> ( x1 x2 -- flag )`

`= ( x1 x2 -- flag )`

`min ( n1 n2 -- n1|n2 )` Keeps smaller of top two items

`max ( n1 n2 -- n1|n2 )` Keeps greater of top two items

`umin ( u1 u2 -- u1|u2 )` Keeps unsigned smaller

`umax ( u1 u2 -- u1|u2 )` Keeps unsigned greater

### Double-Comparisons

[double.s](../../../sw/components/forth/double.s)

`du> ( ud1 ud2 -- flag )`

`du< ( ud1 ud2 -- flag )`

`d> ( d1 d2 -- flag )`

`d< ( d1 d2 -- flag )`

`d0< ( d -- flag )`

`d0= ( d -- flag )`

`d<> ( d1 d2 -- flag )`

`d= ( d1 d2 -- flag )`

### Specials

[calculation.s](../../../sw/components/forth/calculations.s)

`slt ( u1 u2 -- 0 | 1 )` Set if less than

`sltu ( u1 u2 -- 0 | 1 )` Set if less than, unsigned

## Number base  (exactly ANS)

[calculation.s](../../../sw/components/forth/calculations.s)

`binary ( -- )` Sets base to 2

`decimal ( -- )` Sets base to 10

`hex ( -- )` Sets base to 16

`base ( -- a-addr )` Base variable address

## Memory access  (subtle differences to ANS, special cpu-specific extensions)

[memory.s](../../../sw/components/forth/memory.s)
[compiler.s](../../../sw/components/forth/compiler.s)
[compiler-memory.s](../../../sw/components/forth/compiler-memory.s)
[double.s](../../../sw/components/forth/double.s)

`move ( c-addr1 c-addr2 u -- )` Moves u Bytes in Memory

`fill ( c-addr u c )` Fill u Bytes of Memory with value c

`cbit@ ( mask c-addr -- flag )` Test BIts in byte-location

`hbit@ ( mask h-addr -- flag )` Test BIts in halfword-location

`bit@ ( mask a-addr -- flag )` Test BIts in word-location

`cxor! ( mask c-addr -- )` Toggle bits in byte-location

`hxor! ( mask h-addr -- )` Toggle bits in halfword-location

`xor! ( mask a-addr -- )` Toggle bits in word-location

`cbic! ( mask c-addr -- )` Clear BIts in byte-location

`hbic! ( mask h-addr -- )` Clear BIts in halfword-location

`bic! ( mask a-addr -- )` Clear BIts in word-location

`cbis! ( mask c-addr -- )` Set BIts in byte-location

`hbis! ( mask h-addr -- )` Set BIts in halfword-location

`bis! ( mask a-addr -- )` Set BIts in word-location

`2constant name  ( ud|d -- )` Makes a double constant.

`constant name  ( u|n -- )` Makes a single constant.

`2variable name  ( ud|d -- )` Makes an initialized double variable

`variable name  ( n|n -- )` Makes an initialized single variable

`nvariable name  ( n1*u|n n1 -- )` Makes an initialized variable with specified size of n1 words Maximum is 15 words

`buffer: name    ( u -- )` Creates a buffer in RAM with u bytes length

`2@ ( a-addr -- ud|d )` Fetches double number from memory

`2! ( ud|d a-addr -- )` Stores double number in memory

`@ ( a-addr -- u|n )` Fetches single number from memory

`! ( u|n a-addr -- )` Stores single number in memory

`+! ( u|n a-addr -- )` Add to memory location

`h@ ( h-addr -- u )` Fetches halfword from memory

`h@signed ( h-addr -- n )` Fetches halfword with sign extension

`h! ( u h-addr )` Stores halfword in memory

`h+! ( u|n h-addr -- )` Add to halfword memory location

`c@ ( c-addr -- char )` Fetches byte from memory

`c@signed ( c-addr -- n )` Fetches byte with sign extension

`c! ( char c-addr )` Stores byte in memory

`c+! ( u|n a-addr -- )` Add to byte memory location

## Comments

[strings.s](../../../sw/components/forth/strings.s)

`( Comment )` Ignore Comment

`\ Comment` Comment to end of line

## Strings and formatted output (exactly ANS, some logical extensions)

### String routines

[strings.s](../../../sw/components/forth/strings.s)
[numberstrings.s](../../../sw/components/forth/numberstrings.s)
[numberoutput.s](../../../sw/components/forth/numberoutput.s)
[istr.fs](../../../fs/forth/istr.fs)
[utils.fs](../../../fs/forth/utils.fs)

`type ( c-addr length -- )` Prints a string.

`s" Hello"       ( -- c-addr length )`
- Core: Compiles a string and gives back its address and length when executed.
- istr.fs: Adds execution mode behavior.

`." Hello"       ( -- )`
- Core: Compiles a string to be printed when executed.
- istr.fs: Adds exection mode behavior.

`cr ( -- )` Emits line feed

`bl ( -- 32 )` ASCII code for Space

`space ( -- )` Emits space

`spaces ( n -- )` Emits n spaces if n is positive

`compare ( caddr-1 len-1 c-addr-2 len-2 -- flag )` Compares two strings

`/string (addr u n -- addr' u')` Move string pointer forward by n and reduce string length by n.

```
number ( c-addr length -- 0 )
                       -- n 1 )
                       -- n-low n-high 2 )
```
- Tries to convert a string to a number.

### Counted string routines

`ctype ( cstr-addr -- )` Prints a counted string.

`c" Hello"       ( -- cstr-addr )` Compiles a counted string and gives back its address when executed.

`cexpect ( cstr-addr maxlength -- )` Read input into a counted string.

`count ( cstr-addr -- c-addr length )` Convert counted string into addr-length string

### Pictured numerical output

`.digit ( u -- char )` Converts a digit to a char

`digit ( char -- u true | false )` Converts a char to a digit

`[char] * ( -- char )` Compiles code of following char when executed

`char * ( -- char )` gives code of following char

`hold ( char -- )` Adds character to pictured number output buffer from the front.

`sign ( n -- )` Add a minus sign to pictured number output buffer, if n is negative

`#S ( ud1|d1 -- 0 0 )` Add all remaining digits from the double length number to output buffer

`# ( ud1|d1 -- ud2|d2 )` Add one digit from the double length number to output buffer

`#> ( ud|d -- c-addr len )` Drops double-length number and finishes pictured numeric output ready for type

`<# ( -- )` Prepare pictured number output buffer

`u. ( u -- )` Print unsigned single number

`. ( n -- )` Print single number

`ud. ( ud -- )` Print unsigned double number

`d. ( d -- )` Print double number

`hold< ( char -- )` Adds character to pictured number output buffer from behind.

`f#S ( n-comma1 -- n-comma2 )` Adds 32 comma-digits to number output

`f# ( n-comma1 -- n-comma2 )` Adds one comma-digit to number output

`f. ( df -- )` Prints a fixpoint number with 32 fractional digits

`f.n ( df n -- )` Prints a fixpoint number with n fractional digits

`hex. ( u -- )` Prints 32 bit unsigned in hex base, needs emit only. This is independent of number subsystem.

## Deep insights

[deepinsight.s](../../../sw/components/forth/deepinsight.s)

`words ( -- )` Prints list of defined words.

`.s ( many -- many )` Prints stack contents, signed

`u.s ( many -- many )` Prints stack contents, unsigned

`h.s ( many -- many )` Prints stack contents, unsigned, hex

`.rs ( many -- many )` Prints return stack contents

`unused ( -- u )` Get current amount of free memory

## User input and its interpretation (exactly ANS, some logical extensions)

[query.s](../../../sw/components/forth/query.s)
[token.s](../../../sw/components/forth/token.s)
[interpreter.s](../../../sw/components/forth/interpreter.s)

`query ( -- )` Fetches user input to input buffer

`tib ( -- cstr-addr )` Input buffer

`current-source ( -- addr )` Double-Variable which contains source

`setsource ( c-addr len -- )` Change source

`source ( -- c-addr len )` Current source

`>in ( -- addr )` Variable with current offset into source

`accept ( c-addr maxlength -- length )` Read input into a string.

`token ( -- c-addr len )` Cuts one token out of input buffer

`parse ( char -- c-addr len )` Cuts anything delimited by char out of input buffer

`evaluate ( any addr len -- any )` Interpret given string

`interpret ( any -- any )` Execute, compile, fold, optimize...

`quit ( many -- )` (R: many -- ) Resets Stacks

`hook-quit ( -- a-addr )` Hook for changing the inner quit loop

`(quit) ( any -- any )` Standard REPL, default for hook-quit

## Dictionary expansion  (exactly ANS, some logical extensions)

[compiler.s](../../../sw/components/forth/compiler.s)
[compiler-memory.s](../../../sw/components/forth/compiler-memory.s)
[calculations.s](../../../sw/components/forth/calculations.s)
[utils.fs](../../../fs/forth/utils.fs)

`alignto ( a power -- a )` Align an address to a power of two

`align ( -- )` Word-align dictionary pointer

`halign ( -- )` Halfword-align dictionary pointer

`aligned ( c-addr -- a-addr )` Advances to next aligned address

`haligned ( c-addr -- a-addr )` Advances to next half-word aligned address

`cell+ ( x -- x+4 )` Add size of one cell

`cells ( n -- 4*n )` Calculate size of n cells

`allot ( n -- )` Tries to advance Dictionary Pointer by n bytes. Aborts, if not enough space available

`here ( -- a-addr|h-addr )` Gives current position in Dictionary

`(dp) ( -- a-addr )` Variable: Dictionary pointer

`(latest) ( -- a-addr )` Variable: Latest definition

`, ( u|n -- )` Appends a single number to dictionary

`h, ( u|n -- )` Appends a halfword to dictionary

`c, ( u|n -- )` Appends a byte to dictionary

`compiletoemem? ( -- ? )` Currently compiling into emem?

`compiletoemem ( -- )` Makes emem the target for compiling

`compiletoimem ( -- )` Makes imem the target for compiling

`addrinimem? ( addr -- flag )` Location in imem?

`addrinemem? ( addr -- flag )` Location in emem?

`string, ( c-addr len -- )` Inserts a string of maximum 255 characters without runtime

`literal, ( u|n -- )` Compiles a literal with runtime

`inline, ( a-addr -- )` Inlines the chosen subroutine

`call, ( a-addr -- )` Compiles a call to a subroutine

`ret, ( -- )` Compiles a ret opcode

`ramvar-here ( -- a-addr )` Gives current RAM management pointer

`dictionarystart ( -- a-addr )` Current entry point for dictionary search

`dictionarynext ( a-addr -- a-addr flag )` Scans dictionary chain and returns true if end is reached.

`skipdefinition ( addr -- addr* )` Skip after the next ret opcode

`(sp) ( -- a-addr)` Variable to compare data stack pointer before and after compilation of definitions

`registerliteral, ( x register -- )` Generate shortest possible sequence to get x into given register.

## Flags and inventory

Note that `[immediate]` needs to be *inside* of the definition, not after the `;`. There is no `immediate` Word variant that goes *after* the definition.

[compiler-memory.s](../../../sw/components/forth/compiler-memory.s)
[compiler.s](../../../sw/components/forth/compiler.s)

`smudge ( -- )` Makes current definition visible, takes care of proper ending

`[inline] ( -- )` Makes current definition inlineable.

`[noframe] ( -- )` No need to push/pop link register when compiling this definition.

`[immediate] ( -- )` Makes current definition immediate.

`[compileonly] ( -- )` Makes current definition compileonly.

`setflags ( x -- )` Sets Flags with a mask. This isn't immediate, but for flash, place it inside your definition !

`(create) name   ( -- )` Creates and links a new invisible dictionary header that does nothing.

`find ( c-addr len -- a-addr flags )` Searches for a String in Dictionary. Gives back flags, which are different to ANS !

`hook-find ( -- a-addr )` Hook for redirecting find

`(find) ( c-addr len -- a-addr flags )` Default find implementation

`[0-foldable] ( -- )` Current word becomes foldable with zero constants

`[1-foldable] ( -- )` Current word becomes foldable with one constants

`[2-foldable] ( -- )` Current word becomes foldable with two constants

...

`[7-foldable] ( -- )` Current word becomes foldable with 7   constants

## Compiler essentials  (subtle differences to ANS)

[compiler.s](../../../sw/components/forth/compiler.s)
[buildsdoes.s](../../../sw/components/forth/buildsdoes.s)

`execute ( a-addr -- )` Calls subroutine

`recurse ( -- )` Lets the current definition call itself

`' name          ( -- a-addr )` Tries to find name in dictionary gives back executable address

`['] name        ( -- a-addr)` Tick that compiles the executable address of found word as literal

`postpone name   ( -- )` Helps compile immediate words.

`does> ( -- )` executes: ( -- a-addr ) Gives address to where you have stored data.

`<builds ( -- )` Makes Dictionary header and reserves space for special call.

`create name     ( -- )` Create a definition with default action which cannot be changed later. Equivalent to: `create <builds does>`

`state ( -- a-addr )` Address of state variable

`] ( -- )` Switch to compile state

`[ ( -- )` Switch to execute state

`; ( -- )` Finishes new definition

`: name          ( -- )` Opens new definition

## Control structures (exactly ANS)

Control structures are immediate and compileonly.

Internally they have complicated compile-time stack effects.

### Decisions

[controlstructures.s](../../../sw/components/forth/controlstructures.s)

```
flag if ... then
flag if ... else ... then
```
 - This is the common `flag if ... [else ...]` then structure.

```
ahead ... then
```
- Unconditionally jump over `...` to `then`.

Case:

[case.s](../../../sw/components/forth/case.s)

```
n case
     m1   of ... endof
     m2   .. ... .....
   flag  ?of ... endof
    all others
  endcase
```

`case ( n -- n )` Begins case structure

`of ( m -- )` Compares m with n, choose this if n=m

`?of ( n flag -- )` Flag-of, for custom comparisons

`endof ( -- )` End of one possibility

`endcase ( n -- )` Ends case structure, discards n

### Indefinite Loops

[controlstructures.s](../../../sw/components/forth/controlstructures.s)

```
begin ... again
begin ... flag until
begin ... flag while ... repeat
begin ... flag while ... flag while ... repeat ... else ... then
```

`repeat ( -- )` Finish of a middle-flag-checking loop.

`while ( flag -- )` Check a flag in the middle of a loop

`until ( flag -- )` begin ... flag until

`again ( -- )` begin ... again is an endless loop

`begin ( -- )`

### Definite Loops

[doloop.s](../../../sw/components/forth/doloop.s)
[compiler.s](../../../sw/components/forth/compiler.s)

```
limit index   do ... [one or more leave(s)] ... loop
             ?do ... [one or more leave(s)] ... loop
              do ... [one or more leave(s)] ... n +loop
             ?do ... [one or more leave(s)] ... n +loop
```

`k ( -- u|n )` Gives third  loop index

`j ( -- u|n )` Gives second loop index

`i ( -- u|n )` Gives innermost loop index

`unloop (R: old-limit old-index -- )` Drops innermost loop structure, pops back old loop structures to loop registers

`exit ( -- )` Returns from current definition. Compiles a ret opcode.

`leave ( -- ) (R: old-limit old-index -- )` Leaves current innermost loop promptly

`+loop ( u|n -- ) (R: unchanged | old-limit old-index -- )` Adds number to current loop index register and checks whether to continue or not

`loop ( -- ) (R: unchanged | old-limit old-index -- )` Increments current loop index register by one and checks whether to continue or not.

`?do ( Limit Index -- ) (R: unchanged | -- old-limit old-index )` Begins a loop if limit and index are not equal

`do ( Limit Index -- ) (R: -- old-limit old-index )` Begins a loop

## C Foreign Function Interface

`c-fun Define: ( fun "name" -- ) Execute: ( i*x -- j*x )`
- Register a C function so it can be called from Forth. See [Forth Calling C](c-ffi.md#forth-calling-c).

