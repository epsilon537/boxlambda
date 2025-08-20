#libtmt-based terminal emulator

add_library(terminal)

target_sources(terminal
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/libtmt/tmt.c
)

target_compile_options(terminal
 PRIVATE
  -O2 -g
)

target_include_directories(terminal
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sub/libtmt/
)

