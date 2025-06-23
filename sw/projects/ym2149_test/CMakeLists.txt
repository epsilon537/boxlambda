#
# YM2149 Test - RAM Build
#
add_executable(ym2149_test_ram
 EXCLUDE_FROM_ALL
 ym2149_test.c
)

target_compile_options(ym2149_test_ram
 PRIVATE
  -O2
)

target_include_directories(ym2149_test_ram
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ym2149_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(ym2149_test_ram gpio riscv bootstrap)

#
# YM2149 Test - Flash Build
#
add_executable(ym2149_test_flsh
 EXCLUDE_FROM_ALL
 ym2149_test.c
)

target_compile_options(ym2149_test_flsh
 PRIVATE
  -O2
)

target_include_directories(ym2149_test_flsh
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ym2149_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ym2149_test_flsh gpio riscv bootstrap)

add_flash_sw_target(ym2149_test_flsh)
