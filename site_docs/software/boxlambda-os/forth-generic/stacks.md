# Stack Objects

Simple stack object with cell-sized elements.

`stack-create ( n "name" -- )`

- Create an empty stack object of n elements.

`stack-push ( x stack -- )`

- Push x on the stack. May raise `x-stack-obj-overflow`.

`stack-pop ( stack -- x )`

- Pop x from the stack. May raise `x-stack-obj-underflow`.

`stack-depth ( stack -- n )`

- Number of elements currently on the stack

`stack-max-depth ( stack -- n )`

- Returns the maximum number of elements on the stack.

`stack-free ( stack -- n )`

- Returns the number of free elements left on the stack.

`stack-base ( stack -- stack-base-addr )`

- Returns the base address of the stack.

`stack-top ( stack -- stack-top-addr )`

- Returns the address of the next free element on the stack. Note that this may point one cell past the end of the stack max. depth.

`>stack-top ( addr stack -- )`

- Set the stack top to the given address. E.g. `<stack> dup stack-base swap >stack-top` resets the stack. Raises `x-stack-obj-underflow` or `x-stack-obj-overflow` if given address is outside the stack object range.

`stack-reset ( stack -- )`

- Reset the stack.

`stack-find ( x stack -- addr|0 )`

- Find x in the stack and return its address or 0 if not found.

## Exceptions

`x-stack-obj-overflow`

- Stack object overflow.

`x-stack-obj-underflow`

- Stack object underflow.

