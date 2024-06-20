add_library(vera)

target_sources(vera
    PRIVATE
        vera_hal.c
)

target_compile_options(vera
 PRIVATE
  -O2 -g
)

target_include_directories(vera
    PUBLIC
        ${PROJECT_SOURCE_DIR}/sub/vera_wishbone/misc
        ${PROJECT_SOURCE_DIR}/sw/components/bootstrap #For interrupts.h
        ${CMAKE_CURRENT_LIST_DIR}
)


