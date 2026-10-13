# Arrays

[array.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/array.fs)

`array <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n cells with given name. `i <name>` returns the address of the i-th cell.

`carray <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n bytes with given name. `i <name>` returns the address of the i-th byte.

