add_library(flashdrvr)

#Flash driver sources plus some supporting files originally from ZipCPU's qspiflash project
#https://github.com/ZipCPU/qspiflash

target_sources(flashdrvr
    PRIVATE
    flashdrvr.cpp
    byteswap.cpp
)

target_compile_options(flashdrvr
 PRIVATE
  -O2 -g
)

target_include_directories(flashdrvr
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
)

