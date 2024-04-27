#PicoRV_DMA system test program

add_executable(dual_port_ram_test
 EXCLUDE_FROM_ALL
 dual_port_ram_test.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(dual_port_ram_test
 picorv_wordcopy_burst)

target_compile_options(dual_port_ram_test
 PRIVATE
  -O2 -g
)

target_include_directories(dual_port_ram_test
    PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_internal_create_mem_file(dual_port_ram_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(dual_port_ram_test
 picorv_dma)
