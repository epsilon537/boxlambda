#Reset test program

add_executable(reset_test
 EXCLUDE_FROM_ALL
 reset_test.cpp
)

target_compile_options(reset_test
 PRIVATE
  -O2 -g
)

target_include_directories(reset_test
  PRIVATE
  ${PROJECT_SOURCE_DIR}/registers/generated/
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(reset_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(reset_test
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(reset_test sdram gpio riscv bootstrap)

