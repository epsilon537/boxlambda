add_library(embedded-cli)

target_sources(embedded-cli
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/embedded-cli/lib/src/embedded_cli.c
)

target_compile_options(embedded-cli
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(embedded-cli
    PUBLIC
       ${PROJECT_SOURCE_DIR}/sub/embedded-cli/lib/include/
)

