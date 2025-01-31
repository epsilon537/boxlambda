#
# DDR Test RAM build - 25MHz variant
#
add_compile_definitions(SYS_CLK_25MHZ)

add_executable(ddr_test_25mhz_ram
    EXCLUDE_FROM_ALL
      ../ddr_test/ddr_test.c
      ../ddr_test/memtest.c
)

if(CMAKE_BUILD_TYPE STREQUAL "verilator")
  target_compile_definitions(ddr_test_25mhz_ram PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(ddr_test_25mhz_ram PRIVATE -DMEMTEST_SIZE=32768)
endif()

target_compile_options(ddr_test_25mhz_ram
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_25mhz_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(ddr_test_25mhz_ram sdram gpio riscv)

#
# DDR Test Flash build
#
add_executable(ddr_test_25mhz_flsh
    EXCLUDE_FROM_ALL
      ../ddr_test/ddr_test.c
      ../ddr_test/memtest.c
)

target_compile_definitions(ddr_test_25mhz_flsh PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))

target_compile_options(ddr_test_25mhz_flsh
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_25mhz_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ddr_test_25mhz_flsh sdram_25mhz gpio riscv)

add_flash_sw_target(ddr_test_25mhz_flsh)

