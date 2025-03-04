#
# DDR Test RAM build
#
add_executable(ddr_test_ram
    EXCLUDE_FROM_ALL
      ddr_test.c
      memtest.c
)

target_compile_definitions(ddr_test_ram PRIVATE -DMEMTEST_SIZE=32768)

target_compile_options(ddr_test_ram
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_ram ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(ddr_test_ram sdram gpio riscv bootstrap)

#
# DDR Test Flash build
#
add_executable(ddr_test_flsh
    EXCLUDE_FROM_ALL
      ddr_test.c
      memtest.c
)

target_compile_definitions(ddr_test_flsh PRIVATE -DMEMTEST_SIZE=32768)

target_compile_options(ddr_test_flsh
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test_flsh ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_boot.ld)

target_link_libraries(ddr_test_flsh sdram gpio riscv bootstrap)

add_flash_sw_target(ddr_test_flsh)

