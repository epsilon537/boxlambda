#Reset test program - RAM Build

add_executable(reset_test_ram
 EXCLUDE_FROM_ALL
 reset_test.c
)

target_compile_options(reset_test_ram
 PRIVATE
  -O2 -g
)

target_include_directories(reset_test_ram
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

link_and_create_image(reset_test_ram
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(reset_test_ram sdram gpio riscv bootstrap)

#Reset test program - Flash Build

add_executable(reset_test_flsh
 EXCLUDE_FROM_ALL
 reset_test.c
)

target_compile_options(reset_test_flsh
 PRIVATE
  -O2 -g
)

target_include_directories(reset_test_flsh
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

link_and_create_image(reset_test_flsh
 ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(reset_test_flsh sdram gpio riscv bootstrap)

add_flash_sw_target(reset_test_flsh)
