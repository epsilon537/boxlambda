\ --- SPIFLASH
\ --- 32-bit register bitfield accessors

$100000c0 constant SPIFLASH_BASE_ADDR


\ CTRL - SPIFlash Control Register
SPIFLASH_BASE_ADDR $0 + constant SPIFLASH_CTRL_ADDR

\ -- Address Mask Offset
SPIFLASH_CTRL_ADDR $ff #0 bitfield@ SPIFLASH_CTRL_DATA@
SPIFLASH_CTRL_ADDR $ff #0 bitfield! SPIFLASH_CTRL_DATA!
SPIFLASH_CTRL_ADDR $100 #8 bitfield@ SPIFLASH_CTRL_CS_N@
SPIFLASH_CTRL_ADDR $100 #8 bitfield! SPIFLASH_CTRL_CS_N!
