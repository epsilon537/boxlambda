# Interpreting Console Input

The following diagram illustrates the Forth core interpreting console input:

[![Interpreting Console Input.](../../assets/interpreting-console-input.png)](../../assets/interpreting-console-input.png)

*Forth Core Interpreting Console Input.*

## Words

[query.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/query.s)
[token.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/token.s)
[interpreter.s](https://github.com/epsilon537/boxlambda/blob/develop/sw/components/forth/interpreter.s)

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

