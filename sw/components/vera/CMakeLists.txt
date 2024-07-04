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
        ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(vera interrupts)


