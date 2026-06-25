\ Automatically generated from master definition. Do not edit.

$00000000        constant IMEM_BASE \ Internal Memory Base
262144           constant IMEM_SIZE_BYTES \ 256 KB
1048576          constant BOOTLOADER_IMG_SIZE_BYTES \ Bootloader Image Size
2097152          constant APP_SW_IMG_SIZE_BYTES \ Application Software Image Size
$11000000        constant SPIFLASH_BASE \ SPI Flash Memory Base
16777216         constant SPIFLASH_SIZE_BYTES \ 16 MB Flash Array
$11000000        constant SPIFLASH_BITSTREAM_BASE \ FPGA Bitstream Base address
4194304          constant SPIFLASH_BITSTREAM_SIZE_BYTES \ 4 MB Bitstream Size Allocation
$11500000        constant SPIFLASH_BOOTLOADER_BASE \ Bootloader Base Address. Note the 0x100000 gap from the end of the bitstream
1048576          constant SPIFLASH_BOOTLOADER_SIZE_BYTES
$11600000        constant SPIFLASH_SW_BASE \ Application Software Image Base Address
2097152          constant SPIFLASH_SW_SIZE_BYTES
$11800000        constant SPIFLASH_NVDATA_BASE \ Non-Volatile Data Base Addess
4194304          constant SPIFLASH_NVDATA_SIZE_BYTES \ 4 MB
$12040000        constant VERA_VRAM_BASE \ VERA Video RAM Base
131072           constant VERA_VRAM_SIZE_BYTES \ 128 KB
$12001000        constant VERA_SPRITE_RAM_BASE
1024             constant VERA_SPRITE_RAM_SIZE_BYTES
$12002000        constant VERA_PALETTE_RAM_BASE
512              constant VERA_PALETTE_RAM_SIZE_BYTES
$12003000        constant VERA_CAPTURE_RAM_BASE
2560             constant VERA_CAPTURE_RAM_SIZE_BYTES
$20000000        constant SDRAM_BASE
268435456        constant SDRAM_SIZE_BYTES \ 256 MB
$2ff00000        constant RAM_DISK_BASE
1048576          constant RAM_DISK_SIZE_BYTES \ 1 MB