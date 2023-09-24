add_library(fatfs)

#FatFs source plus two source files from sdspi tying FatFs to the SDSPI controller
#or in other words, implementing the 'Media Access Interface' for FatFs.
target_sources(fatfs
    PRIVATE
    ../../../sub/fatfs/source/ff.c
    ../../../sub/fatfs/source/ffsystem.c
    ../../../sub/fatfs/source/ffunicode.c     
    ../../../sub/sdspi/sw/diskio.c
    ../../../sub/sdspi/sw/sdcard.c
)

target_compile_definitions(fatfs
	PRIVATE
		-DSDSPI_CPU_IS_LITTLE_ENDIAN
)

#We have to turn on optimizations or we won't fit in the on-chip RAM link map.
target_compile_options(fatfs
	PRIVATE
		-O2 -g
)

target_include_directories(fatfs
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sub/fatfs/source #Paths to include files users of fatfs are expected to use.
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)
