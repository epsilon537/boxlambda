add_library(mcycle)

target_sources(mcycle
    PRIVATE
    mcycle.c
)

target_compile_options(mcycle
 PRIVATE
  -O2 -g
)

#Paths to include files users of usb_hid are expected to use.
target_include_directories(mcycle
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
    PRIVATE
        ${PROJECT_SOURCE_DIR}/sw/components/platform/
)
