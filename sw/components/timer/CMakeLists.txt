add_library(timer)

target_sources(timer
    PRIVATE
    timer.c
)

target_compile_options(timer
 PRIVATE
  -O2 -g
)

target_include_directories(timer
    PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}
    ${PROJECT_SOURCE_DIR}/registers/generated/
)

