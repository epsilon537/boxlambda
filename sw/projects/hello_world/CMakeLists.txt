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

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(hello_world
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(hello_world
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(hello_world gpio riscv bootstrap)

add_flash_sw_target(hello_world)

