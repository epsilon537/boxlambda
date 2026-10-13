# Forth Stack Notation Examples

I've been using the following stack notations, going by example:

## Stack signature of a regular Word

`: foo ( addr len c -- n1 n2 )`

`Foo` takes three items from the data stack, with `c` as TOS.
`Foo` puts two items on the data stack, with `n2` as TOS.

## Stack item conventions:

`addr`: an address

`len`: a length.

An `addr len` pair typically specifies a memory area of `len` bytes starting at address `addr`. A typical use case is passing strings.

`n`: a number

`u`: an unsigned number

`c`: a character / byte

`f`: a boolean flag (0=false, non-zero=true)

`any` : a variable number of arguments is taken from or put on the data stack. For example, `: printf ( any addr u -- )`.

I often use short descriptive names for stack items: `buf`, `buflen`, `fil`, etc.

## Stack signature of a Word taking data from the Input Stream

- `: foo ( "text" n -- )`

`Foo` takes text from the input stream. An accompanying comment clarifies how much text to take: up to a specific delimiter, a token, one character...
`Foo` takes one number from the data stack. It does not put any items on the data stack.

- `: foo" ( "text" -- )`

A variant of the previous example. `Foo` takes text from the input stream up to a `"` delimiter.

## Stack Signature of a Word with a Compile-Time and Run-Time Phase

- `: foo (compile-time: "token" -- ) (run-time: -- addr )`

`Foo` is an Immediate. At compile-time, it takes a token from the input stream. It does not take anything from or put anything on the data stack.
At compile-time, `foo` writes code which at run-time has the stack effect `( -- addr )`, i.e. the compiled code puts an item on the data stack.

## Stack comments:

```
: foo
...
rot >r ( buf buflen R: fil )
...
;
```

After executing `rot >r`, `buf` and `buflen` are on the data stack (`buflen`=TOS), and `fil` is on the return stack.

