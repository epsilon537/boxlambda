# Conditional Execution/Compilation

[ifdef.fs](../../../../fs/forth/boxkern-includes/ifdef.fs)

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

