#bootstrap is an Object Library, not a regular archive. We want to make sure vectors.S is included in the build,
#even if nobody is referencing it.
add_library(bootstrap OBJECT)

target_sources(bootstrap
    PRIVATE
        vectors.S
        crt0.c
        stdio_to_uart.c
        ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/uart.c
)

#Paths to include files users of bootstrap are expected to use.
target_include_directories(bootstrap
    PUBLIC
        ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc
        ${CMAKE_CURRENT_LIST_DIR}
)

target_compile_options(bootstrap
 PRIVATE
  -g
  -O2
  -mno-save-restore
  -fno-tree-loop-distribute-patterns
)
