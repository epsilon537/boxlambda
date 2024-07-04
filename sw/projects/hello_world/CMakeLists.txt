add_executable(hello_world
 EXCLUDE_FROM_ALL
    hello.c
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(hello_world ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(hello_world gpio riscv)
