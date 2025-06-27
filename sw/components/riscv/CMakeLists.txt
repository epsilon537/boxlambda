add_library(riscv)

target_sources(riscv
    PRIVATE
    mcycle.cpp
)

target_compile_options(riscv
 PRIVATE
  -O2 -g -ffunction-sections
)

target_include_directories(riscv
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
