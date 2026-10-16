# Bit Fields

  [bitfield.fs](../../../../fs/forth/boxkern-includes/bitfield.fs)

Register bit field accessors.

`bitfield@ ( address mask offset "name" -- )`

- Creates a bitfield *getter* reading from 32-bit register at address the bits identified by mask and offset.

`bitfield! ( address mask offset "name" -- )`

- Creates a bitfield *setter* writing to 32-bit register at address the bits identified by mask and offset.

`bitfield16@ ( address mask offset "name" -- )`

- Creates a bitfield *getter* reading from 16-bit register at address the bits identified by mask and offset.

`bitfield16! ( address mask offset "name" -- )`

- Creates a bitfield *setter* writing to 16-bit register at address the bits identified by mask and offset.

Example usage:

```
VERA_L0_CONFIG_ADDR $4 #2 bitfield@ VERA_L0_CONFIG_BITMAPMODE@
VERA_L0_CONFIG_ADDR $4 #2 bitfield! VERA_L0_CONFIG_BITMAPMODE!
VERA_L0_CONFIG_ADDR $8 #3 bitfield@ VERA_L0_CONFIG_T256C@
VERA_L0_CONFIG_ADDR $8 #3 bitfield! VERA_L0_CONFIG_T256C!
VERA_L0_CONFIG_ADDR $30 #4 bitfield@ VERA_L0_CONFIG_MAP_WIDTH@
VERA_L0_CONFIG_ADDR $30 #4 bitfield! VERA_L0_CONFIG_MAP_WIDTH!
```

