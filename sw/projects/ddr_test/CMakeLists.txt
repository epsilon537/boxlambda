#
# DDR Test
#
add_executable(ddr_test
    EXCLUDE_FROM_ALL
      ddr_test.cpp
      memtest.cpp
)

target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=32768)

target_compile_options(ddr_test
    PRIVATE
        -O2 -g
)

target_include_directories(ddr_test
    PUBLIC
    ${PROJECT_SOURCE_DIR}/sw/components/memmap/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_image(ddr_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_imem_boot.ld)

target_link_libraries(ddr_test sdram gpio riscv bootstrap)

add_flash_sw_target(ddr_test)
