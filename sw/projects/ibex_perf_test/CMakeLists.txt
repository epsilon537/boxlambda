#
# Ibex Performance Test
#
add_executable(ibex_perf_test
 EXCLUDE_FROM_ALL
 ibex_perf_test.cpp
)

target_compile_options(ibex_perf_test
 PRIVATE
  -O2
)

target_include_directories(ibex_perf_test
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ibex_perf_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(ibex_perf_test gpio riscv vera bootstrap timer interrupts)

