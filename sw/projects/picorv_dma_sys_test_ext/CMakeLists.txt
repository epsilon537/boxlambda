#PicoRV_DMA system test program - RAM Build

add_executable(picorv_dma_sys_test_ext_ram
 EXCLUDE_FROM_ALL
 picorv_dma_sys_test_ext.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(
 picorv_dma_sys_test_ext_ram
 picorv_wordcopy_single
 picorv_bytecopy_single
 picorv_wordcopy_burst
 picorv_bytecopy_burst)

target_compile_options(picorv_dma_sys_test_ext_ram
 PRIVATE
  -O2 -g
)

target_include_directories(picorv_dma_sys_test_ext_ram
    PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
  ${PROJECT_SOURCE_DIR}/sw/components/platform/
)

link_and_create_image(picorv_dma_sys_test_ext_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(picorv_dma_sys_test_ext_ram picorv_dma vera sdram gpio riscv bootstrap)

#PicoRV_DMA system test program - Flash Build

add_executable(picorv_dma_sys_test_ext_flsh
 EXCLUDE_FROM_ALL
 picorv_dma_sys_test_ext.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(
 picorv_dma_sys_test_ext_flsh
 picorv_wordcopy_single
 picorv_bytecopy_single
 picorv_wordcopy_burst
 picorv_bytecopy_burst)

target_compile_options(picorv_dma_sys_test_ext_flsh
 PRIVATE
  -O2 -g
)

target_include_directories(picorv_dma_sys_test_ext_flsh
    PRIVATE
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
  ${PROJECT_SOURCE_DIR}/sw/components/platform/
)

link_and_create_image(picorv_dma_sys_test_ext_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(picorv_dma_sys_test_ext_flsh picorv_dma vera sdram gpio riscv bootstrap)

add_flash_sw_target(picorv_dma_sys_test_flsh)
