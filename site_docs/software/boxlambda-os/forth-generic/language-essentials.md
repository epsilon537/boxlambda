# Forth Language Essentials

## Constants and Units

[units.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/units.fs)

`max-uint`

- Maximum value for an unsigned integer.

`max-int`

- Maximum value for a signed integer.

`min-int`

- Minimum value for a signed integer.

`cells+  ( x n -- x + n * Cell )`

- Add the size of `n` cells to `x`.

`cell+ ( x -- x+4 )`

- Add size of one cell.

`cells ( n -- 4*n )`

- Calculate size of n cells.

`cell ( -- n )`

- Cell size in bytes.

`cell/ ( n -- n' )`

- Divide by cell size.

`chars  ( u -- u )`

- Returns the size of `u` characters, which is equal to `u`. Used for clarity, to put a unit behind a number. For example, `8 Chars`.

`char  ( a -- a + 1 )`

- Increment `a` by one character.

## Range Related Words

[range.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/range.fs)

`span ( addr len -- start end )`

- Convert addr len to start-end address span.

`bounds ( addr len -- end start )`

- Convert addr len to end-start address span.

`within ( n low high -- flag )`

- True if n is within the range [low..high[.

## Terminal-IO

[terminal.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/terminal.s)
[terminalhooks.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/terminalhooks.s)

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

[stackjugglers.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/stackjugglers.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

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

[double.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/double.s)

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

[stackjugglers.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/stackjugglers.s)

`sp@ ( -- a-addr )`

- Fetch data stack pointer.

`sp! ( a-addr -- )`

- Store data stack pointer.

`rp@ ( -- a-addr )`

- Fetch return stack pointer.

`rp! ( a-addr -- )`

- Store return stack pointer.

## Logic and Bit Manipulation

[logic.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/logic.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

Shifts decode the lowest 5 bits only on RISC-V. Therefore, ar/r/lshift behaves like "31 and ar/r/lshift". 32 lshift does nothing.

`arshift ( x1 u -- x2 )`

- Arithmetic right-shift of `u` bit-places.

`rshift ( x1 u -- x2 )`

- Logical right-shift of `u` bit-places.

`lshift ( x1 u -- x2 )`

- Logical left-shift of `u` bit-places.

`shr ( x1 -- x2 )`

- Logical right-shift of one bit-place.

`shl ( x1 -- x2 )`

- Logical left-shift of one bit-place.

`ror ( x1 -- x2 )`

- Logical right-rotation of one bit-place.

`rol ( x1 -- x2 )`

- Logical left-rotation of one bit-place.

`1<< ( u -- u' )`

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

## Math

### Single Number Math (exactly ANS, some logical extensions)

[multiplydivide.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/multiplydivide.s)
[calculations.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/calculations.s)

`u/mod ( u1 u2 -- u3 u4 )`

- 32/32 = 32 rem 32 Division. u1 / u2 = u4 remainder u3.

`/mod ( n1 n2 -- n3 n4 )`

- n1 / n2 = n4 rem n3.

`mod ( n1 n2 -- n3 )`

- n1 / n2 = remainder n3.

`/ ( n1 n2 -- n3 )`

- n1 / n2 = n3.

`* ( u1|n1 u2|n2 -- u3|n3 )`

- 32\*32 = 32 Multiplication.

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

- Arithmetic left-shift 1 position.

`2/ ( n1 -- n2 )`

- Arithmetic right-shift 1 position.

`4/ ( n1 -- n2 )`

- Arithmetic right-shift 2 positions.

`8/ ( n1 -- n2 )`

- Arithmetic right-shift 3 positions.

`log2 ( x - log2(x) )`

- Returns the bit position of the highest bit set to 1 in x. Note: returns 0 if x is 0.

`abs ( n -- u )`

- Absolute value.

`negate ( n1 -- n2 )`

- Negate.

`- ( u1|n1 u2|n2 -- u3|n3 )`

- Subtraction.

`+ ( u1|n1 u2|n2 -- u3|n3 )`

- Addition.

### Double Number Math (exactly ANS, some logical extensions)

[double.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/double.s)
[multiplydivide.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/multiplydivide.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

`um+ ( u1 u2 -- u carry )`

- Unsigned addition with carry.

`um* ( u1 u2 -- ud )`

- 32\*32 = 64 Multiplication.

`ud* ( ud1|d1 ud2|d2 -- ud3|d3 )`

- 64\*64 = 64 Multiplication.

`udm* ( ud1 ud2 -- ud3-Low ud4-High )`

- 64\*64=128 Multiplication.

`um/mod ( ud u1 -- u2 u3 )`

- ud / u1 = u3 remainder u2.

`ud/mod ( ud1 ud2 -- ud3 ud4 )`

- 64/64 = 64 rem 64 Division ud1 / ud2 = ud4 remainder ud3.

`m+ ( d n -- d' )`

- Add n to d.

`m* ( n1 n2 -- d )`

- n1 \* n2 = d.

`m/mod ( d  n1 -- n2 n3 )`

- d / n1 = n3 remainder r2.

`d/mod ( d1 d2 -- d3 d4 )`

- d1 / d2 = d4 remainder d3.

`d/ ( d1 d2 -- d3 )`

- d1 / d2 = d3.

`*/ ( n1 n2 n3 -- n4 )`

- n1 \* n2 / n3 = n4.

`u*/ ( u1 u2 u3 -- u4 )`

- u1 \* u2 / u3 = u4.

`*/mod ( n1 n2 n3 -- n4 n5 )`

- n1 \* n2 / n3 = n5 remainder n4.

`u*/mod ( u1 u2 u3 -- u4 u5 )`

- u1 \* u2 / u3 = u5 remainder u4.

`d2* ( d1 -- d2 )`

- Arithmetic left-shift.

`d2/ ( d1 -- d2 )`

- Arithmetic right-shift.

`dshl ( ud1 -- ud2 )`

- Logical left-shift, same as d2\*.

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

[double.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/double.s)

`f/ ( df1 df2 -- df3 )`

- Division of two fixed-point numbers.

`f* ( df1 df2 -- df3 )`

- Multiplication.

## Comparisons (exactly ANS, some logical extensions)

### Single Comparisons

[comparisons.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/comparisons.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

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

[double.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/double.s)

`du> ( ud1 ud2 -- flag )`

`du< ( ud1 ud2 -- flag )`

`d> ( d1 d2 -- flag )`

`d< ( d1 d2 -- flag )`

`d0< ( d -- flag )`

`d0= ( d -- flag )`

`d<> ( d1 d2 -- flag )`

`d= ( d1 d2 -- flag )`

### Specials

[calculation.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/calculations.s)

`slt ( u1 u2 -- 0 | 1 )`

- Set if less than.

`sltu ( u1 u2 -- 0 | 1 )`

- Set if less than, unsigned.

## Number base

[calculation.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/calculations.s)

`binary ( -- )`

- Sets base to 2.

`decimal ( -- )`

- Sets base to 10.

`hex ( -- )`

- Sets base to 16.

`base ( -- a-addr )`

- Base variable address.

## Memory access

[memory.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/memory.s)
[compiler.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler.s)
[compiler-memory.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler-memory.s)
[double.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/double.s)

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

`setbit ( n bitvalue bitpos -- n' )`

- In number n, set bit position to bit value.

`2constant ( ud|d "name" -- )`

- Makes a double constant.

`constant name  ( u|n "name" -- )`

- Makes a single constant.

`immediate-constant create ( u|n "name" -- )`

- Like constant, but the created constant acts as an immediate.

`2variable ( ud|d "name" -- )`

- Makes an initialized double variable.

`variable ( n|n "name" -- )`

- Makes an initialized single variable.

`nvariable ( n1*u|n n1 "name" -- )`

- Makes an initialized variable with specified size of n1 words. Maximum is 15 words.

`buffer: ( u "name" -- )`

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

[strings.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/strings.s)

`( Comment )`

- Ignore Comment.

`\ Comment`

- Comment to end of line.

## Deep insights

[deepinsight.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/deepinsight.s)

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

## Dictionary expansion

[compiler.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler.s)
[compiler-memory.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler-memory.s)
[calculations.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/calculations.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

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

`allot ( n -- )`

- Tries to advance Dictionary Pointer by n bytes. Aborts, if not enough space available.

`here ( -- a-addr|h-addr )`

- Gives current position in Dictionary.

`(dp) ( -- a-addr )`

- Variable: Dictionary pointer.

`(latest) ( -- a-addr )`

- Variable: Latest definition. Equivalent to `(current) @`. See [WordLists](namespaces.md#word-lists).

`, ( u|n -- )`

- Appends a single number to dictionary.

`h, ( u|n -- )`

- Appends a halfword to dictionary.

`c, ( u|n -- )`

- Appends a byte to dictionary.

`forget ( "word" -- )`

- Forget the given word.

`del ( -- )`

- Delete the latest definition.

`forgetall ( -- )`

- Forget everything except the Forth Core.

`compileto>` ( -- f )

- Retrieve compile-to state.

`>compileto` ( f -- )

- Store compile-to state.

`compileto-save ( -- )`

- Save the current compile-to state.

`compileto-restore ( -- )`

- Restore the compile-to state.

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

- Current entry point for dictionary search, i.e. `(latest) @`.

`dictionarynext ( a-addr -- a-addr flag )`

- Fetches the next entry in the dictionary chain. Returns true if end is reached.

`skipdefinition ( addr -- addr* )`

- Skip after the next ret opcode.

`(sp) ( -- a-addr)`

- Variable to compare data stack pointer before and after compilation of definitions.

`registerliteral, ( x register -- )`

- Generate shortest possible sequence to get x into given register.

## Flags and inventory

Note that `[immediate]` needs to be _inside_ of the definition, not after the `;`. There is no `immediate` Word variant that goes _after_ the definition.

[compiler-memory.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler-memory.s)
[compiler.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler.s)

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

- Sets Flags of latest definition.

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

[compiler.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler.s)
[buildsdoes.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/buildsdoes.s)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)

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

`compile-or-execute ( xt -- )`

- Depending on execution state, compile a call to or execute xt.

## Control structures

Control structures are immediate and compile-only.

Internally they have complicated compile-time stack effects.

### Decisions

[controlstructures.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/controlstructures.s)

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

[case.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/case.s)

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

[controlstructures.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/controlstructures.s)

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

### Definite Loops

[doloop.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/doloop.s)
[compiler.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/compiler.s)

```
limit index   do ... [one or more leave(s)] ... loop
             ?do ... [one or more leave(s)] ... loop
              do ... [one or more leave(s)] ... n +loop
             ?do ... [one or more leave(s)] ... n +loop
```

`k ( -- u|n )`

- Gives third loop index.

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

