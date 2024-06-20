#bootstrap is an Object Library, not a regular archive. We want to make sure vectors.S is included in the build,
#even if nobody is referencing it.
add_library(bootstrap OBJECT)

target_sources(bootstrap
    PRIVATE
        vectors.S
        crt0.c
        stdio_to_uart.c
        interrupts.c
)

#Paths to include files users of bootstrap are expected to use.
target_include_directories(bootstrap
    PUBLIC
        ${CMAKE_CURRENT_LIST_DIR}
)

#We don't want to used the save/restore prologue/epilogue function optimization in the bootstrap component.
#These functions end up in different code sections and end up confusing the crt0 early startup code.
#We also turns off the tree-loop-distribute-patterns for the bootstrap component. We don't want the compiler
#to detect memory copy / memory set patterns in the early bootstrap code and replace them with calls to
#memcpy/memset.
target_compile_options(bootstrap
 PRIVATE
  -g
  -O2
  -mno-save-restore
  -fno-tree-loop-distribute-patterns
)

target_link_libraries(bootstrap uart)
