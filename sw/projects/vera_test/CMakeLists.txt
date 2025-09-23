#
# VERA Test
#
add_executable(vera_test
 EXCLUDE_FROM_ALL
 vera_test.cpp
)

target_compile_options(vera_test
 PRIVATE
  -O2 -g
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(vera_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(vera_test
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(vera_test vera sdram gpio riscv interrupts bootstrap)

add_flash_sw_target(vera_test)
