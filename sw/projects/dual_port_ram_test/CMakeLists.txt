#
# Dual Port RAM test RAM build
#

add_executable(dual_port_ram_test_ram
 EXCLUDE_FROM_ALL
 dual_port_ram_test.c
)

target_compile_options(dual_port_ram_test_ram
 PRIVATE
  -O2 -g
)

target_include_directories(dual_port_ram_test_ram
    PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
)

link_and_create_image(dual_port_ram_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(dual_port_ram_test_ram gpio riscv bootstrap)

#
# Dual Port RAM test Flash build
#
add_executable(dual_port_ram_test_flsh
 EXCLUDE_FROM_ALL
 dual_port_ram_test.c
)

target_compile_options(dual_port_ram_test_flsh
 PRIVATE
  -O2 -g
)

target_include_directories(dual_port_ram_test_flsh
    PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}
)

link_and_create_image(dual_port_ram_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(dual_port_ram_test_flsh gpio riscv bootstrap)

add_flash_sw_target(dual_port_ram_test_flsh)
