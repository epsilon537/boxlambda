# ZeptoForth Structs

[struct.fs](https://github.com/epsilon537/boxlambda/blob/develop/fs/forth/struct.fs)

`begin-structure ( "name" -- addr offset )`

- Begin declaring a structure.

`end-structure ( addr offset -- )`

- Finish declaring a structure.

`+field: ( offset size "name" -- offset )`

- Create an arbitrary-sized field.

`cfield: ( offset "name" -- offset )`

- Create a byte-sized field.

`hfield: ( offset "name" -- offset )`

- Create a halfword-sized field.

`field: ( offset "name" -- offset )`

- Create a cell-sized field.

`2field: ( offset "name" -- offset )`

- Create a double cell-sized field.

Example:

```
begin-structure fil-buf
  field: .fil
  field: .buf
end-structure

create fil-buf0 fil-buf allot
create fil-buf1 fil-buf allot

fil-buf0 .fil @ fil-buf0 .buf @ 256 f_read
```


