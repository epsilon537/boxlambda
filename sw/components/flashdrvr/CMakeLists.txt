add_library(flashdrvr)

#Flash driver source plus some supporting files from ZipCPU's OpenArty project
#https://github.com/ZipCPU/openarty/tree/master

target_sources(flashdrvr
    PRIVATE
    ../../../sub/qspiflash/sw/flashdrvr.cpp
	byteswap.cpp
)

target_compile_definitions(flashdrvr
	PUBLIC
		-DFLASH_ACCESS -DR_FLASHCFG=0x100000C0 -DR_FLASH=0x11000000 -DR_FLASH_LEN=0x01000000
)

#We have to turn on optimizations or we won't fit in the on-chip RAM link map.
target_compile_options(flashdrvr
	PRIVATE
		-O2 -g
)

target_include_directories(flashdrvr
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
	../../../sub/qspiflash/sw/
)
