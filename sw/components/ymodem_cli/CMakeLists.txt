#Ymodem CLI commands

add_library(ymodem_cli)

target_sources(ymodem_cli
    PRIVATE
    ymodem_cli.cpp
)

target_compile_options(ymodem_cli
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(ymodem_cli
    PUBLIC
       ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(ymodem_cli embedded_cli ymodem fatfs)
