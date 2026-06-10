\ --- SPIFLASH

$100000c0 constant SPIFLASH_BASE_ADDR

\ CTRL - SPIFlash Control Register
$0 constant SPIFLASH_CTRL_ADDR

\ CTRL.DATA - If control port is active, written byte value is sent out to SPI slave, top bit first. After write, may be read to retrieve return data byte.
8 constant SPIFLASH_CTRL_DATA_WIDTH
0 constant SPIFLASH_CTRL_DATA_LSB
$ff constant SPIFLASH_CTRL_DATA_MASK
\ CTRL.CS_N - 1/0 de/activates the control port.
1 constant SPIFLASH_CTRL_CS_N_WIDTH
8 constant SPIFLASH_CTRL_CS_N_LSB
$100 constant SPIFLASH_CTRL_CS_N_MASK
