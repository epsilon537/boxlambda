#
# Hello World RAM Build
#

add_executable(hello_world_ram
 EXCLUDE_FROM_ALL
    hello.c
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world_ram
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(hello_world_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(hello_world_ram gpio riscv)

#
# Hello World Flash Build
#

add_executable(hello_world_flsh
 EXCLUDE_FROM_ALL
    hello.c
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world_flsh
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(hello_world_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(hello_world_flsh gpio riscv)

add_flash_sw_target(hello_world_flsh)

