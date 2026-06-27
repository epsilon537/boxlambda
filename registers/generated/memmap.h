#ifndef MEMMAP_H
#define MEMMAP_H

/* Automatically generated from master definition. Do not edit. */

#define IMEM_BASE                        (0x00000000U) // Internal Memory Base
#define IMEM_SIZE_BYTES                  262144 // 256 KB
#define BOOTLOADER_IMG_SIZE_BYTES        1048576 // Bootloader Image Size
#define APP_SW_IMG_SIZE_BYTES            2097152 // Application Software Image Size
#define SPIFLASH_BASE                    (0x11000000U) // SPI Flash Memory Base
#define SPIFLASH_SIZE_BYTES              16777216 // 16 MB Flash Array
#define SPIFLASH_BITSTREAM_BASE          (0x11000000U) // FPGA Bitstream Base address
#define SPIFLASH_BITSTREAM_SIZE_BYTES    4194304 // 4 MB Bitstream Size Allocation
#define SPIFLASH_BOOTLOADER_BASE         (0x11500000U) // Bootloader Base Address.
#define SPIFLASH_BOOTLOADER_SIZE_BYTES   1048576
#define SPIFLASH_SW_BASE                 (0x11600000U) // Application Software Image Base Address
#define SPIFLASH_SW_SIZE_BYTES           2097152
#define SPIFLASH_NVDATA_BASE             (0x11800000U) // Non-Volatile Data Base Addess
#define SPIFLASH_NVDATA_SIZE_BYTES       4194304 // 4 MB
#define VERA_VRAM_BASE                   (0x12040000U) // VERA Video RAM Base
#define VERA_VRAM_SIZE_BYTES             131072 // 128 KB
#define VERA_SPRITE_RAM_BASE             (0x12001000U)
#define VERA_SPRITE_RAM_SIZE_BYTES       1024
#define VERA_PALETTE_RAM_BASE            (0x12002000U)
#define VERA_PALETTE_RAM_SIZE_BYTES      512
#define VERA_CAPTURE_RAM_BASE            (0x12003000U)
#define VERA_CAPTURE_RAM_SIZE_BYTES      2560
#define SDRAM_BASE                       (0x20000000U)
#define SDRAM_SIZE_BYTES                 268435456 // 256 MB
#define RAM_DISK_BASE                    (0x2FF00000U)
#define RAM_DISK_SIZE_BYTES              1048576 // 1 MB

#endif /* MEMMAP_H */