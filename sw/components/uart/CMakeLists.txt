add_library(uart)

target_sources(uart
    PRIVATE
    uart.c
)

target_compile_options(uart
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(uart
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
