# Strings and formatted output

## String routines

[strings.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/strings.s)
[numberstrings.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/numberstrings.s)
[numberoutput.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/numberoutput.s)
[istr.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/istr.fs)
[utils.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/utils.fs)
[tonumber.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/tonumber.fs)

`type ( c-addr length -- )`

- Prints a string.

`s" text"       ( -- c-addr length )`

- Core: Compile-only Immediate. Compiles the string given on input stream. The compiled code, when executed, puts the string address and length on the data stack.

- `istr.fs`: Adds interpretive execution mode behavior. Take string data from input stream, put string address and length on the data stack.

`." text" ( -- )`

- Core: Compiles a string to be printed when executed.

- `istr.fs`: Adds interpretive execution mode behavior. Take string data from input stream and print it.

`istr-free-all ( -- )`

- Free (release) all strings in the interactive string table.

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

## Counted string routines

`ctype ( cstr-addr -- )`

- Prints a counted string.

`c" Hello"       ( -- cstr-addr )`

- Compiles a counted string and gives back its address when executed.

`cexpect ( cstr-addr maxlength -- )`

- Read input into a counted string.

`count ( cstr-addr -- c-addr length )`

- Convert counted string into addr-length string.

## Escaped Strings

[escstr.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/escstr.fs)

`escape-string ( addr len -- addr' len' )`

- In given string, substitute escape codes according to table below.

`esc-s" ( parses up to "  -- addr len )`

- Parse string from input string and substitute escape codes according to table below.

`.esc-s" ( parses up to "  -- )`
Parse string from input string, substitute escape codes according to table below, and print.

| Escape Code | Substitution |
| ----------- | ------------ |
| \t          | Tab          |
| \n          | LF           |
| \r          | CR           |
| \e          | ESC          |
| \\          | \            |
| '           | "            |

## Formatted Numerical Output

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

- Add a minus sign to pictured number output buffer if n is negative.

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

- Prints 32-bit unsigned in hex base, needs emit only. This is independent of number subsystem.

## jkotlinski Printf-Style Printing and Formatting

[printf.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/boxkern-includes/printf.fs)

`sprintf ( n*x addr1 u1 addr2 -- addr2 u3 )`

- Prints n\*x into buffer addr2 using the format string at addr1 u. addr2 u3 is the resulting string.

`printf ( n*x c-addr u -- )`

- Prints n\*x using the format string at caddr u. The format string consists of ordinary characters (except %), which are copied verbatim to the destination buffer, and conversion specifications. Conversion specifications have the following structure:

- Introductory % character.
- An optional '-' that signifies left justification.
- An optional '0' that pads with zeros instead of spaces.
- An optional decimal integer value specifying the minimum field width.
- A conversion format specifier.

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


