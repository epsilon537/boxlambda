add_library(lz4)

target_sources(lz4
    PRIVATE
    lz4.c
)

target_compile_options(lz4
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(lz4
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
