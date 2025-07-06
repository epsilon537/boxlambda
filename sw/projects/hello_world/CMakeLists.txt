#
# Hello World
#

add_executable(hello_world
 EXCLUDE_FROM_ALL
    hello.cpp
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world
 PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(hello_world ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(hello_world gpio riscv bootstrap)

add_flash_sw_target(hello_world)

