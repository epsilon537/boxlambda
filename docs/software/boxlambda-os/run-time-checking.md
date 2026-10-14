# Run-Time Checking

[run-time-checking.fs]()
[except.fs]()

Run-Time Checking tools.

## Assert Blocks

`?assert ( f -- )`

- Raise `x-assert` exception if f is false.

`xassert-enable`

- Variable. Set to 1/0 to enable/disable xassert checking. Default: enabled.

`xassert{ <this-must-be-true> }xassert`

- Put an assert statement between xassert{ ... }xassert. Any number of statements can be put between the { }, but they must end with a flag. If the flag is false, an `x-assert` exception is raised. `xassert{...}xassert` blocks are compiled out if `xassert-enable` is set to 0.

### Exceptions

`x-assert`

- Assert failed.

## Struct Typechecking

`rttc-struct`

- Variable. Set to 1/0 to enable/disable run-time type checking on structs. Default: enabled.

```
typechecker 
( init-type typecheck run-time: struct-inst -- ) 
( typecheck run-time: struct-inst -- struct-inst )
```

- Defining Word: Create a typechecker instance for structs.

`init-type ( struct-inst "typechecker" -- )`

- Set the struct instance to the given type.

Usage example:

```
begin-structure tilemap-struct
  field:  .base
  hfield: .width
  hfield: .height
  cfield: .type
end-structure

typechecker tm-typecheck

\ Initialize the tilemap object.
\ ( tilemap -- )
: init 
  dup tilemap-struct 0 fill
  init-type tm-typecheck
;

\ Retrieve map width from the tilemap object.
\ ( tilemap -- width )
: width@
  tm-typecheck \ print message with wordname/location if typecheck fails.
  .width h@ ;
```

### Exceptions

`x-typecheck-failed`

- Struct typecheck failed.

## Stack Checking

`stack-checking-enable`

- Variable. Set to 1/0 to enable/disable stack checking. Default: disabled.


`stack-checker ( #in #out -- )`

- Invoke as follows (example):

```
( n1 n2 -- n3 )
: foo
  [ 2 1 stack-checker ]
  ...
;
```

i.e. in execution mode create a stack-checker instance and specify the number of input and output params. If after the stack checked Word's execution the stack doesn't have the expected depth, a failure will be reported and execution stops.

