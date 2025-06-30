add_library(vera)

target_sources(vera
    PRIVATE
        vera_hal.cpp
)

target_compile_options(vera
 PRIVATE
  -O2 -g
)

target_include_directories(vera
    PUBLIC
      ${PROJECT_SOURCE_DIR}/registers/generated/
      ${CMAKE_CURRENT_LIST_DIR}
)

target_link_libraries(vera interrupts)


