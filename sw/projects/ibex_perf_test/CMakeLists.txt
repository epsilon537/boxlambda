#
# Ibex Performance Test - RAM Build
#
add_executable(ibex_perf_test_ram
 EXCLUDE_FROM_ALL
 ibex_perf_test.c
)

target_compile_options(ibex_perf_test_ram
 PRIVATE
  -O2
)

target_include_directories(ibex_perf_test_ram
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/i2c/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ibex_perf_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(ibex_perf_test_ram gpio riscv vera bootstrap)

#
# ibex_perf Test - Flash Build
#
add_executable(ibex_perf_test_flsh
 EXCLUDE_FROM_ALL
 ibex_perf_test.c
)

target_compile_options(ibex_perf_test_flsh
 PRIVATE
  -O2
)

target_include_directories(ibex_perf_test_flsh
  PRIVATE
  ${PROJECT_SOURCE_DIR}/sw/components/i2c/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ibex_perf_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ibex_perf_test_flsh gpio riscv vera bootstrap)

add_flash_sw_target(ibex_perf_test_flsh)
