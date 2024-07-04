add_library(riscv)

target_sources(riscv
    PRIVATE
    mcycle.c
)

target_compile_options(riscv
 PRIVATE
  -O2 -g
)

target_include_directories(riscv
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)
