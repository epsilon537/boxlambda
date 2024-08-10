#Arduino Time Library ported to BoxLambda

add_library(timelib)

target_sources(timelib
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/Time/Time.cpp
)

target_compile_options(timelib
 PRIVATE
  -O2 -g
)

target_include_directories(timelib
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sub/Time/
)

target_link_libraries(timelib riscv)
