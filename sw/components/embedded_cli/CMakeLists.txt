#Embedded-CLI library

add_library(embedded_cli)

target_sources(embedded_cli
    PRIVATE
    embedded_cli_setup.c
    ${PROJECT_SOURCE_DIR}/sub/embedded-cli/lib/src/embedded_cli.c
)

target_compile_options(embedded_cli
 PRIVATE
  -O2 -g -ffunction-sections
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(embedded_cli
    PUBLIC
       ${CMAKE_CURRENT_LIST_DIR}
       ${PROJECT_SOURCE_DIR}/sub/embedded-cli/lib/include/
)

target_link_libraries(embedded_cli uart)

