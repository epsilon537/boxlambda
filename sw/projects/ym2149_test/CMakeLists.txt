#
# YM2149 Test
#
add_executable(ym2149_test
 EXCLUDE_FROM_ALL
 ym2149_test.cpp
)

target_compile_options(ym2149_test
 PRIVATE
  -O2
)

target_include_directories(ym2149_test
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(ym2149_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(ym2149_test
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(ym2149_test gpio riscv bootstrap)

