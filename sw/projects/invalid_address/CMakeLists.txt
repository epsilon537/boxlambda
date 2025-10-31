#
# WB Error Handling Test
#

add_executable(invalid_address
 EXCLUDE_FROM_ALL
    invalid_address.cpp
)

target_compile_options(invalid_address
 PRIVATE -g)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  link_and_create_image(invalid_address ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)
else()
  link_and_create_image(invalid_address
  ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_ddr_to_imem_boot.ld)
endif()

target_link_libraries(invalid_address gpio riscv bootstrap)

