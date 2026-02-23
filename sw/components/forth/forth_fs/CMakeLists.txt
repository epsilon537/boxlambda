# The Forth Filesystem
add_library(forth_fs)

target_sources(forth_fs
  PRIVATE
    fs_ffi.cpp
)

target_compile_options(forth_fs
  PRIVATE
    -O2 -g -ffunction-sections
)

target_include_directories(forth_fs
  PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(forth_fs fatfs forth_core )
