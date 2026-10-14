# Arrays

[array.fs](../../../../fs/forth/array.fs)
[utils.fs]()

`array <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n cells with given name. `i <name>` returns the address of the i-th cell.

`harray <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n halfwords with given name. `i <name>` returns the address of the i-th halfword.

`carray <name> ( compile time: n -- ) ( run time: i -- addr)`

- Create an array of n bytes with given name. `i <name>` returns the address of the i-th byte.

`find-in ( x list n -- addr | 0 )`

- Find x in list of n consecutive cells. Return its address or 0 if not found.

