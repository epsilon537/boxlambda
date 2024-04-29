add_library(flashdrvr)

#Flash driver source plus some supporting files from ZipCPU's qspiflash project
#https://github.com/ZipCPU/qspiflash

target_sources(flashdrvr
    PRIVATE
    ../../../sub/qspiflash/sw/flashdrvr.cpp
    byteswap.cpp
)

#R_FLASHCFG: Address of the configuation register of the flash core.
#R_FLASH: Base address of flash memory.
#R_FLASH_LEN: Length of flash memory.
target_compile_definitions(flashdrvr
 PUBLIC
  -DR_FLASHCFG=0x100000C0 -DR_FLASH=0x11000000 -DR_FLASH_LEN=0x01000000
)

target_compile_options(flashdrvr
 PRIVATE
  -O2 -g
)

target_include_directories(flashdrvr
    PUBLIC
    ../../../sub/qspiflash/sw/
    ${CMAKE_CURRENT_LIST_DIR}
)
