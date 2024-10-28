# mem_fs_cli is an embedded_cli client component providing convenience commands
# for file system access and memory management.

add_library(mem_fs_cli)

target_sources(mem_fs_cli
    PRIVATE
    mem_fs_cli.cpp
)

target_compile_options(mem_fs_cli
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(mem_fs_cli
    PUBLIC
       ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(mem_fs_cli embedded_cli fatfs)
