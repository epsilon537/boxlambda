add_library(fatfs)

target_sources(fatfs
    PRIVATE
    ../../../sub/fatfs/source/diskio.c
    ../../../sub/fatfs/source/ff.c
    ../../../sub/fatfs/source/ffsystem.c
    ../../../sub/fatfs/source/ffunicode.c     
    ../../../sub/sdspi/sw/diskio.c
    ../../../sub/sdspi/sw/sdcard.c

)

#Paths to include files users of fatfs are expected to use.
target_include_directories(fatfs
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sub/fatfs/source
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)
