#
# DDR Test RAM build
#
add_executable(ddr_test_ram
    EXCLUDE_FROM_ALL
      ddr_test.c
      memtest.c
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  target_compile_definitions(ddr_test_ram PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(ddr_test_ram PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

target_compile_options(ddr_test_ram
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(ddr_test_ram sdram gpio riscv)

#
# DDR Test Flash build
#
add_executable(ddr_test_flsh
    EXCLUDE_FROM_ALL
      ddr_test.c
      memtest.c
)

target_compile_definitions(ddr_test_flsh PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))

target_compile_options(ddr_test_flsh
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ddr_test_flsh sdram gpio riscv)

add_flash_sw_target(ddr_test_flsh)

