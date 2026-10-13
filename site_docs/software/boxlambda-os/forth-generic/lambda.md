# ZeptoForth Lambda Anonymous Functions

[lambda.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/lambda.fs)

`[: ( -- )`

- Immediate. Begin Lambda.

`;] ( compile-time: -- ) ( run-time: -- xt )`

- Compile-only Immediate. End Lambda definition. At run-time, returns the _xt_ of the code between `[:` and `:]`.

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

