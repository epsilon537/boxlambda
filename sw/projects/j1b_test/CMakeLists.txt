#J1B test program

add_executable(j1b_test_ram
 EXCLUDE_FROM_ALL
 j1b_test.cpp
)

target_compile_options(j1b_test_ram
 PRIVATE
  -O2 -g
)

target_include_directories(j1b_test_ram
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/reset/
)

#reset_test is linked as CMEM memory image.
link_and_create_mem_file(j1b_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(j1b_test_ram sdram gpio riscv j1b_hal)

add_executable(j1b_test
 EXCLUDE_FROM_ALL
 j1b_test.cpp
)

target_compile_options(j1b_test
 PRIVATE
  -O2 -g
)

target_include_directories(j1b_test
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/reset/
)

#spiflash_test is linked as a flash memory image.
link_and_create_mem_file(j1b_test
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(j1b_test sdram gpio riscv j1b_hal)

add_flash_sw_target(j1b_test)

