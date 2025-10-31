# --- SPIFLASH

.equ SPIFLASH_BASE_ADDR, 0x100000c0

# CTRL - SPIFlash Control Register
.equ SPIFLASH_CTRL_ADDR, 0x0

# CTRL.DATA - If control port is active, written byte value is sent out to SPI slave, top bit first. After write, may be read to retrieve return data byte.
.equ SPIFLASH_CTRL_DATA_WIDTH, 8
.equ SPIFLASH_CTRL_DATA_LSB, 0
.equ SPIFLASH_CTRL_DATA_MASK, 0xff
# CTRL.CS_N - 1/0 de/activates the control port.
.equ SPIFLASH_CTRL_CS_N_WIDTH, 1
.equ SPIFLASH_CTRL_CS_N_LSB, 8
.equ SPIFLASH_CTRL_CS_N_MASK, 0x100
