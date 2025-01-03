#
# Dual Port RAM test RAM build
#

add_executable(dual_port_ram_test_ram
 EXCLUDE_FROM_ALL
 dual_port_ram_test.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(dual_port_ram_test_ram
 picorv_wordcopy_burst)

target_compile_options(dual_port_ram_test_ram
 PRIVATE
  -O2 -g
)

target_include_directories(dual_port_ram_test_ram
    PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_image(dual_port_ram_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(dual_port_ram_test_ram picorv_dma gpio riscv)

#
# Dual Port RAM test Flash build
#
add_executable(dual_port_ram_test_flsh
 EXCLUDE_FROM_ALL
 dual_port_ram_test.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(dual_port_ram_test_flsh
 picorv_wordcopy_burst)

target_compile_options(dual_port_ram_test_flsh
 PRIVATE
  -O2 -g
)

target_include_directories(dual_port_ram_test_flsh
    PRIVATE
      ${CMAKE_CURRENT_BINARY_DIR}
      ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_and_create_image(dual_port_ram_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(dual_port_ram_test_flsh picorv_dma gpio riscv)

add_flash_sw_target(dual_port_ram_test_flsh)
