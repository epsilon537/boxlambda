add_library(j1b_hal)

#j1b_hal hardware access layer.
target_sources(j1b_hal
    PRIVATE
    j1b_hal.c
)

target_compile_options(j1b_hal
 PRIVATE
  -O2 -g
)

#Paths to include files users of j1b_hal are expected to use.
target_include_directories(j1b_hal
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
