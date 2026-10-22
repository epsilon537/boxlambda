# Iterators and Anonymous Lists

[iter.fs](../../../../fs/forth/boxkern-includes/iter.fs)

## Iterators

Based on Zeptoforth iterators.

`citer ( addr count xt -- ) ( xt: c -- )`

- Iterate executing an xt over a byte array.

`hiter ( addr count xt -- ) ( xt: h -- )`

- Iterate executing an xt over a halfword array.

`iter ( addr count xt -- ) ( xt: x -- )`

- Iterate executing an xt over a cell array

## Anonymous Lists

`l{ ... }l ( -- addr count )`

- An anonymous list of cells, e.g. `l{ 4 ,  5 , 6 , }l`.

`h{ ... }h ( -- addr count )`

- An anonymous list of halfwords, e.g. `h{ 4 h,  5 h, 6 h, }h`.

`c{ ... }c ( -- addr count )`

- An anonymous list of bytes, e.g. `c{ 4 c,  5 c, 6 c, }c`.

