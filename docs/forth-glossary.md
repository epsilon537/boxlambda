# Forth Word Glossary

This glossary is taken from the Mecrisp Quintus README, with modifications for
BoxLambda.

The coverage of specific platform components such as [Interrupt Handling](forth_irqs.md) and [Time Handling](forth_time.md) is factored out into separate subsections.

Mecrisp-Quintus is case-insensitive, but only for letters 'a' to 'z'.
UTF-8 Unicode encoded characters beyond 7 bit ASCII are case-sensitive.

Words with `(BoxLambda)` in the description have been added or modified as part of
the BoxLambda port of Mecrisp.

## Terminal-IO  (exactly ANS, some logical extensions)

```
        emit?           ( -- Flag ) Ready to send a character ?
        key?            ( -- Flag ) Checks if a key is waiting
        key             ( -- Char ) Waits for and fetches the pressed key
        emit            ( Char -- ) Emits a character.

        hook-emit?      ( -- a-addr ) Hooks for redirecting
        hook-key?       ( -- a-addr )   terminal IO
        hook-key        ( -- a-addr )     on the fly
        hook-emit       ( -- a-addr )

        serial-emit?    ( -- Flag )  Serial interface
        serial-key?     ( -- Flag )    terminal routines
        serial-key      ( -- Char )      as default communications
        serial-emit     ( Char -- )

        hook-pause      ( -- a-addr ) Hook for a multitasker
        pause           ( -- )        Task switch, none for default
```

## Stack Jugglers  (exactly ANS, some logical extensions)

Single-Jugglers:

```
        depth           ( -- +n ) Gives number of single-cell stack items.
        nip             ( x1 x2 -- x2 )
        drop            ( x -- )
        rot             ( x1 x2 x3 -- x2 x3 x1 )
        -rot            ( x1 x2 x3 -- x3 x1 x2 )
        swap            ( x1 x2 -- x2 x1 )
        tuck            ( x1 x2 -- x2 x1 x2 )
        over            ( x1 x2 -- x1 x2 x1 )
        ?dup            ( x -- 0 | x x )
        dup             ( x -- x x )
        pick            ( ... xi+1 xi ... x1 x0 i -- ... x1 x0 xi )
                                  Picks one element from deep below

        >r              ( x -- ) (R: -- x )
        r>              ( -- x ) (R: x -- )
        r@              ( -- x ) (R: x -- x )
        rdrop           (  --  ) (R: x -- )
        rdepth          ( -- +n ) Gives number of return stack items.
        rpick           ( i -- xi ) R: ( ... xi ... x0 -- ... xi ... x0 )
```

Double-Jugglers:        They perform the same for double numbers.

```
        2nip            ( x1 x2 x3 x4 -- x3 x4 )
        2drop           ( x1 x2 -- )
        2rot            ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
        2-rot           ( x1 x2 x3 x4 x5 x6 -- x5 x6 x1 x2 x3 x4 )
        2swap           ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
        2tuck           ( x1 x2 x3 x4 -- x3 x4 x1 x2 x3 x4 )
        2over           ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
        2dup            ( x1 x2 -- x1 x2 x1 x2 )

        2>r             ( x1 x2 -- ) (R: -- x1 x2 )
        2r>             ( -- x1 x2 ) (R: x1 x2 -- )
        2r@             ( -- x1 x2 ) (R: x1 x2 -- x1 x2 )
        2rdrop          ( -- )       (R: x1 x2 -- )
```

Stack pointers:

```
        sp@             ( -- a-addr )  Fetch  data stack pointer
        sp!             ( a-addr -- )  Store  data stack pointer
        rp@             ( -- a-addr )  Fetch return stack pointer
        rp!             ( a-addr -- )  Store return stack pointer
```

## Logic  (exactly ANS, some logical extensions)

Shifts decode the lowest 5 bits only on RISC-V. Therefore, ar/r/lshift behaves
like "31 and ar/r/lshift". 32 lshift does nothing.

```
        arshift         ( x1 u -- x2 ) Arithmetic right-shift of u bit-places
        rshift          ( x1 u -- x2 ) Logical right-shift of u bit-places
        lshift          ( x1 u -- x2 ) Logical  left-shift of u bit-places
        shr             ( x1 -- x2 )   Logical right-shift of one bit-place
        shl             ( x1 -- x2 )   Logical  left-shift of one bit-place
        ror             ( x1 -- x2 )   Logical right-rotation of one bit-place
        rol             ( x1 -- x2 )   Logical  left-rotation of one bit-place
        bic             ( x1 x2 -- x3 ) Bit clear, identical to "not and"
        not             ( x1 -- x2 )   Invert all bits
        xor             ( x1 x2 -- x3 ) Bitwise Exclusive-OR
        or              ( x1 x2 -- x3 ) Bitwise OR
        and             ( x1 x2 -- x3 ) Bitwise AND
        false           ( --  0 ) False-Flag
        true            ( -- -1 ) True-Flag
        clz             ( x1 -- u ) Count leading zeros
```

## Calculus for single numbers  (exactly ANS, some logical extensions)

```
        u/mod           ( u1 u2 -- u3 u4 ) 32/32 = 32 rem 32 Division
                                           u1 / u2 = u4 remainder u3
        /mod            ( n1 n2 -- n3 n4 ) n1 / n2 = n4 rem n3
        mod             ( n1 n2 -- n3 ) n1 / n2 = remainder n3
        /               ( n1 n2 -- n3 ) n1 / n2 = n3
        *               ( u1|n1 u2|n2 -- u3|n3 ) 32*32 = 32 Multiplication
        min             ( n1 n2 -- n1|n2 ) Keeps smaller of top two items
        max             ( n1 n2 -- n1|n2 ) Keeps greater of top two items
        umin            ( u1 u2 -- u1|u2 ) Keeps unsigned smaller
        umax            ( u1 u2 -- u1|u2 ) Keeps unsigned greater
        2-              ( u1|n1 -- u2|n2 ) Subtracts two, optimized
        1-              ( u1|n1 -- u2|n2 ) Subtracts one, optimized
        2+              ( u1|n1 -- u2|n2 ) Adds two, optimized
        1+              ( u1|n1 -- u2|n2 ) Adds one, optimized
        even            ( u1|n1 -- u2|n2 ) Makes even. Adds one if uneven.
        2*              ( n1 -- n2 ) Arithmetic  left-shift
        2/              ( n1 -- n2 ) Arithmetic right-shift
        abs             ( n -- u ) Absolute value
        negate          ( n1 -- n2 ) Negate
        -               ( u1|n1 u2|n2 -- u3|n3 ) Subtraction
        +               ( u1|n1 u2|n2 -- u3|n3 ) Addition
```

## Calculus involving double numbers  (exactly ANS, some logical extensions)

```
        um*             ( u1 u2 -- ud )        32*32 = 64 Multiplication
        ud*             ( ud1|d1 ud2|d2 -- ud3|d3 )  64*64 = 64 Multiplication
        udm*            ( ud1 ud2 -- ud3-Low ud4-High ) 64*64=128 Multiplication

        um/mod          ( ud u1 -- u2 u3 )     ud / u1 = u3 remainder u2
        ud/mod          ( ud1 ud2 -- ud3 ud4 ) 64/64 = 64 rem 64 Division
                                               ud1 / ud2 = ud4 remainder ud3

        m*              ( n1 n2 -- d )         n1 * n2 = d
        m/mod           ( d  n1 -- n2 n3 )     d  / n1 = n3 remainder r2
        d/mod           ( d1 d2 -- d3 d4 )     d1 / d2 = d4 remainder d3
        d/              ( d1 d2 -- d3 )        d1 / d2 = d3
        */              ( n1 n2 n3 -- n4 )     n1 * n2 / n3 = n4
        u*/             ( u1 u2 u3 -- u4 )     u1 * u2 / u3 = u4
        */mod           ( n1 n2 n3 -- n4 n5 )  n1 * n2 / n3 = n5 remainder n4
        u*/mod          ( u1 u2 u3 -- u4 u5 )  u1 * u2 / u3 = u5 remainder u4

        d2*             ( d1 -- d2 ) Arithmetic  left-shift
        d2/             ( d1 -- d2 ) Arithmetic right-shift
        dshl            ( ud1 -- ud2 )  Logical  left-shift, same as d2*
        dshr            ( ud1 -- ud2 )  Logical right-shift

        dabs            ( d -- ud ) Absolute value
        dnegate         ( d1 -- d2 ) Negate
        d-              ( ud1|d1 ud2|d2 -- ud3|d3 ) Subtraction
        d+              ( ud1|d1 ud2|d2 -- ud3|d3 ) Addition
        s>d             ( n -- d ) Makes a signed single number double length

        2arshift        ( d1 u -- d2 ) Arithmetic double right-shift of u bit-places
        2rshift         ( d1 u -- d2 ) Logical    double right-shift of u bit-places
        2lshift         ( d1 u -- d2 ) Logical    double  left-shift of u bit-places
```

## Fixed point numbers

S31.32 fixpoint numbers are written like `3,14159`, i.e. written with a comma
instead of a dot.

Fixpoint numbers are stored ( n-comma n-whole ) and can be handled
like signed double numbers.

```
        f/              ( df1 df2 -- df3 ) Division of two fixpoint numbers
        f*              ( df1 df2 -- df3 ) Multiplication

        hold<           ( char -- )
                        Adds character to pictured number output buffer
                        from behind.
        f#S             ( n-comma1 -- n-comma2 )
                        Adds 32 comma-digits to number output
        f#              ( n-comma1 -- n-comma2 )
                        Adds one comma-digit to number output
        f.              ( df -- )
                        Prints a fixpoint number with 32 fractional digits
        f.n             ( df n -- )
                        Prints a fixpoint number with n fractional digits

        number          ( c-addr length -- 0 )
                                        -- n 1 )
                                        -- n-low n-high 2 )
                        Tries to convert a string to a number.
```

## Comparisons  (exactly ANS, some logical extensions)

Single-Comparisons:

```
        u<=             ( u1 u2 -- flag )  Unsigned comparisions
        u>=             ( u1 u2 -- flag )
        u>              ( u1 u2 -- flag )
        u<              ( u1 u2 -- flag )
        <=              ( n1 n2 -- flag )    Signed comparisions
        >=              ( n1 n2 -- flag )
        >               ( n1 n2 -- flag )
        <               ( n1 n2 -- flag )
        0<              ( n - flag )         Negative ?
        0<>             ( x -- flag )
        0=              ( x -- flag )
        <>              ( x1 x2 -- flag )
        =               ( x1 x2 -- flag )
```

Double-Comparisons:            They perform the same for double numbers.

```
        du>             ( ud1 ud2 -- flag )
        du<             ( ud1 ud2 -- flag )
        d>              ( d1 d2 -- flag )
        d<              ( d1 d2 -- flag )
        d0<             ( d -- flag )
        d0=             ( d -- flag )
        d<>             ( d1 d2 -- flag )
        d=              ( d1 d2 -- flag )
```

Specials:

```
        slt             ( u1 u2 -- 0 | 1 ) Set if less than
        sltu            ( u1 u2 -- 0 | 1 ) Set if less than, unsigned
```

## Number base  (exactly ANS)

```
        binary          ( -- ) Sets base to 2
        decimal         ( -- ) Sets base to 10
        hex             ( -- ) Sets base to 16
        base            ( -- a-addr ) Base variable address
```

## Memory access  (subtle differences to ANS, special cpu-specific extensions)

```
        move            ( c-addr1 c-addr2 u -- ) Moves u Bytes in Memory
        fill            ( c-addr u c ) Fill u Bytes of Memory with value c

        cbit@           ( mask c-addr -- flag ) Test BIts in byte-location
        hbit@           ( mask h-addr -- flag ) Test BIts in halfword-location
        bit@            ( mask a-addr -- flag ) Test BIts in word-location

        cxor!           ( mask c-addr -- ) Toggle bits in byte-location
        hxor!           ( mask h-addr -- ) Toggle bits in halfword-location
        xor!            ( mask a-addr -- ) Toggle bits in word-location

        cbic!           ( mask c-addr -- ) Clear BIts in byte-location
        hbic!           ( mask h-addr -- ) Clear BIts in halfword-location
        bic!            ( mask a-addr -- ) Clear BIts in word-location

        cbis!           ( mask c-addr -- ) Set BIts in byte-location
        hbis!           ( mask h-addr -- ) Set BIts in halfword-location
        bis!            ( mask a-addr -- ) Set BIts in word-location

        2constant name  ( ud|d -- ) Makes a double constant.
        constant  name  ( u|n -- )  Makes a single constant.
        2variable name  ( ud|d -- ) Makes an initialized double variable
        variable  name  ( n|n -- )  Makes an initialized single variable
        nvariable name  ( n1*u|n n1 -- ) Makes an initialized variable with
                                         specified size of n1 words
                                         Maximum is 15 words

        buffer: name    ( u -- ) Creates a buffer in RAM with u bytes length

        2@              ( a-addr -- ud|d ) Fetches double number from memory
        2!              ( ud|d a-addr -- ) Stores double number in memory

        @               ( a-addr -- u|n )  Fetches single number from memory
        !               ( u|n a-addr -- )  Stores single number in memory
        +!              ( u|n a-addr -- )  Add to memory location

        h@              ( h-addr -- u )    Fetches halfword from memory
        h@signed        ( h-addr -- n )    Fetches halfword with sign extension
        h!              ( u h-addr )       Stores halfword in memory
        h+!             ( u|n h-addr -- )  Add to halfword memory location

        c@              ( c-addr -- char ) Fetches byte from memory
        c@signed        ( c-addr -- n )    Fetches byte with sign extension
        c!              ( char c-addr )    Stores byte in memory
        c+!             ( u|n a-addr -- )  Add to byte memory location
```

## Strings and beautiful output (exactly ANS, some logical extensions)

String routines:

```
        type            ( c-addr length -- )
                        Prints a string.

        s" Hello"       Compiles a string and
                        ( -- c-addr length )
                        gives back its address and length when executed.

        ." Hello"       Compiles a string and
                        ( -- )
                        prints it when executed.

        ( Comment )     Ignore Comment
        \ Comment       Comment to end of line

        .( Text )       Immediately print the text between the parentheses.

        cr              ( -- ) Emits line feed
        bl              ( -- 32 ) ASCII code for Space
        space           ( -- ) Emits space
        spaces          ( n -- ) Emits n spaces if n is positive

        compare         ( caddr-1 len-1 c-addr-2 len-2 -- flag )
                        Compares two strings

        accept          ( c-addr maxlength -- length ) Read input into a string.
```

Counted string routines:

```
        ctype           ( cstr-addr -- )
                        Prints a counted string.

        c" Hello"       Compiles a counted string and
                        ( -- cstr-addr )
                        gives back its address when executed.

        cexpect         ( cstr-addr maxlength -- ) Read input into a counted string.

        count           ( cstr-addr -- c-addr length )
                        Convert counted string into addr-length string

        skipstring      ( cstr-addr -- a-addr )
                        Increases the pointer to the aligned end of the string.
```

Pictured numerical output:

```
        .digit          ( u -- char ) Converts a digit to a char
        digit           ( char -- u true | false ) Converts a char to a digit

        [char] *        Compiles code of following char
                        ( -- char ) when executed

        char *          ( -- char ) gives code of following char
        hold            ( char -- ) Adds character to pictured number
                                    output buffer from the front.

        sign            ( n -- ) Add a minus sign to pictured number
                                 output buffer, if n is negative

        #S              ( ud1|d1 -- 0 0 ) Add all remaining digits
                        from the double length number to output buffer
        #               ( ud1|d1 -- ud2|d2 ) Add one digit from the
                        double length number to output buffer
        #>              ( ud|d -- c-addr len )
                        Drops double-length number and finishes
                        pictured numeric output ready for type
        <#              ( -- ) Prepare pictured number output buffer
        u.              ( u -- ) Print unsigned single number
        .               ( n -- ) Print single number
        ud.             ( ud -- ) Print unsigned double number
        d.              ( d -- ) Print double number
```

Deep insights:

```
        words           ( -- ) Prints list of defined words.
        .s              ( many -- many ) Prints stack contents, signed
        u.s             ( many -- many ) Prints stack contents, unsigned
        h.s             ( many -- many ) Prints stack contents, unsigned, hex
        .rs             ( many -- many ) Prints return stack contents
        hex.            ( u -- ) Prints 32 bit unsigned in hex base,
                                 needs emit only.
                                 This is independent of number subsystem.
```

## User input and its interpretation (exactly ANS, some logical extensions)

```
        query           ( -- ) Fetches user input to input buffer
        tib             ( -- cstr-addr ) Input buffer

        current-source  ( -- addr ) Double-Variable which contains source
        setsource       ( c-addr len -- ) Change source
        source          ( -- c-addr len ) Current source
        >in             ( -- addr ) Variable with current offset into source

        token           ( -- c-addr len ) Cuts one token out of input buffer
        parse           ( char -- c-addr len )
                        Cuts anything delimited by char out of input buffer

        evaluate        ( any addr len -- any ) Interpret given string
        interpret       ( any -- any ) Execute, compile, fold, optimize...
        quit            ( many -- ) (R: many -- ) Resets Stacks
        hook-quit       ( -- a-addr ) Hook for changing the inner quit loop
        (quit)          ( any -- any ) Standard REPL, default for hook-quit
```

## Dictionary expansion  (exactly ANS, some logical extensions)

```
        align           ( -- ) Word-align dictionary pointer
        halign          ( -- ) Halfword-align dictionary pointer (BoxLambda)
        aligned         ( c-addr -- a-addr ) Advances to next aligned address
        haligned        ( c-addr -- a-addr ) Advances to next half-word aligned address (BoxLambda)
        cell+           ( x -- x+4 ) Add size of one cell
        cells           ( n -- 4*n ) Calculate size of n cells

        unused          ( -- u ) Get current amount of free memory
        allot           ( n -- ) Tries to advance Dictionary Pointer by n bytes
                                 Aborts, if not enough space available
        here            ( -- a-addr|h-addr )
                        Gives current position in Dictionary
        (dp)            ( -- a-addr ) Variable: Dictionary pointer
        (latest)        ( -- a-addr ) Variable: Latest definition

        ,               ( u|n -- ) Appends a single number to dictionary
        h,              ( u|n -- ) Appends a halfword to dictionary (BoxLamba)
        c,              ( u|n -- ) Appends a byte to dictionary (BoxLambda)

        compiletoemem?  ( -- ? ) Currently compiling into emem ? (BoxLambda)
        compiletoemem   ( -- ) Makes emem the target for compiling (BoxLambda)
        compiletoimem   ( -- ) Makes imem the target for compiling (BoxLambda)

        forget          ( -- ) Forget everything except the Forth Core.

        addrinimem?     ( addr -- flag ) Location in imem ? (BoxLambda)
        addrinemem?     ( addr -- flag ) Location in emem ? (BoxLambda)

        string,         ( c-addr len -- ) Inserts a string of maximum 255 characters without runtime
        literal,        ( u|n -- ) Compiles a literal with runtime
        inline,         ( a-addr -- ) Inlines the chosen subroutine
        call,           ( a-addr -- ) Compiles a call to a subroutine
        ret,            ( -- ) Compiles a ret opcode

        ramvar-here   ( -- a-addr ) Gives current RAM management pointer
        dictionarystart ( -- a-addr ) Current entry point for dictionary search
        dictionarynext  ( a-addr -- a-addr flag )
                        Scans dictionary chain and returns true if end is reached.

        skipdefinition  ( addr -- addr* ) Skip after the next ret opcode
        (sp)            ( -- a-addr) Variable to compare data stack pointer
                                     before and after compilation of definitions

       registerliteral, ( x register -- ) Generate shortest possible sequence
                                          to get x into given register.
```

Can x be encoded as immediate for...

```
        uj-encoding?    ( x -- x false | bitmask true ) ... unconditional jumps
        sb-encoding?    ( x -- x false | bitmask true ) ...   conditional jumps
```

## Flags and inventory

Note that `[immediate]` needs to be *inside* of the definition, not after the `;`. There is no `immediate` Word variant that goes *after* the definition.

```
        smudge          ( -- ) Makes current definition visible,
                               takes care of proper ending
        [inline]        ( -- ) Makes current definition inlineable. (BoxLambda).
        [noframe]       ( -- ) No need to push/pop link register when compiling this definition. (BoxLambda)
        [immediate]     ( -- ) Makes current definition immediate. (BoxLambda)
        [compileonly]   ( -- ) Makes current definition compileonly. (BoxLambda)
        setflags        ( x -- ) Sets Flags with a mask. This isn't immediate,
                               but for flash, place it inside your definition !
        (create) name   ( -- ) Creates and links a new invisible dictionary
                               header that does nothing.
        find            ( c-addr len -- a-addr flags )
                               Searches for a String in Dictionary.
                               Gives back flags, which are different to ANS !
        hook-find       ( -- a-addr ) Hook for redirecting find
        (find)          ( c-addr len -- a-addr flags ) Default find implementation

        [0-foldable]    ( -- ) Current word becomes foldable with zero constants (BoxLambda)
        [1-foldable]    ( -- ) Current word becomes foldable with one constants (BoxLambda)
        [2-foldable]    ( -- ) Current word becomes foldable with two constants (BoxLambda)
        [3-foldable]    ( -- ) Current word becomes foldable with 3   constants (BoxLambda)
            ...
        [7-foldable]    ( -- ) Current word becomes foldable with 7   constants (BoxLambda)
```

## Compiler essentials  (subtle differences to ANS)

```
        execute         ( a-addr -- ) Calls subroutine
        recurse         ( -- ) Lets the current definition call itself
        ' name          ( -- a-addr ) Tries to find name in dictionary
                                      gives back executable address
        ['] name        ( -- a-addr)  Tick that compiles the executable address
                                      of found word as literal
        postpone name   ( -- ) Helps compile immediate words.
        does>           ( -- ) executes: ( -- a-addr )
                               Gives address to where you have stored data.
        <builds         ( -- ) Makes Dictionary header and reserves space
                               for special call.
        create name     ( -- ) Create a definition with default action which
                               cannot be changed later. Use <builds does> instead.
                               Equivalent to : create <builds does> ;
        state           ( -- a-addr ) Address of state variable
        ]               ( -- ) Switch to compile state
        [               ( -- ) Switch to execute state
        ;               ( -- ) Finishes new definition
        : name          ( -- ) Opens new definition
```

## Control structures (exactly ANS)

Internally, they have complicated compile-time stack effects.

Decisions:

```
flag if ... then
flag if ... else ... then

        then            ( -- )           This is the common
        else            ( -- )           flag if ... [else ...] then
        if              ( flag -- )      structure.
        ahead           ( -- )
```

Case:

```
n case
     m1   of ... endof
     m2   .. ... .....
   flag  ?of ... endof
    all others
  endcase

        case            ( n -- n )       Begins case structure
        of              ( m -- )         Compares m with n, choose this if n=m
        ?of             ( n flag -- )    Flag-of, for custom comparisons
        endof           ( -- )           End of one possibility
        endcase         ( n -- )         Ends case structure, discards n
```

Indefinite Loops:

```
begin ... again
begin ... flag until
begin ... flag while ... repeat
begin ... flag while ... flag while ... repeat ... else ... then

        repeat          ( -- ) Finish of a middle-flag-checking loop.

        while           ( flag -- ) Check a flag in the middle of a loop

        until           ( flag -- ) begin ... flag until
                                    loops until flag is true
        again           ( -- )  begin ... again
                                is an endless loop
        begin           ( -- )
```

Definite Loops:

```
limit index   do ... [one or more leave(s)] ... loop
             ?do ... [one or more leave(s)] ... loop
              do ... [one or more leave(s)] ... n +loop
             ?do ... [one or more leave(s)] ... n +loop


        k               ( -- u|n ) Gives third  loop index
        j               ( -- u|n ) Gives second loop index
        i               ( -- u|n ) Gives innermost loop index


        unloop          (R: old-limit old-index -- )
                        Drops innermost loop structure,
                        pops back old loop structures to loop registers

        exit            ( -- ) Returns from current definition.
                               Compiles a ret opcode.

        leave           ( -- ) (R: old-limit old-index -- )
                        Leaves current innermost loop promptly

        +loop           ( u|n -- )
                        (R: unchanged | old-limit old-index -- )
                        Adds number to current loop index register
                        and checks whether to continue or not

        loop            ( -- )
                        (R: unchanged | old-limit old-index -- )
                        Increments current loop index register by one
                        and checks whether to continue or not.

        ?do             ( Limit Index -- )
                        (R: unchanged | -- old-limit old-index )
                        Begins a loop if limit and index are not equal

        do              ( Limit Index -- )
                        (R: -- old-limit old-index )
                        Begins a loop
```

## Misc

```
        risc-v          ( -- ) Welcome message if arch is RISC-V
```

