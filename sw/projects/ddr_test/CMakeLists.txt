add_executable(ddr_test
    EXCLUDE_FROM_ALL
      ddr_test.c
      memtest.c
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
  target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
  target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

target_compile_options(ddr_test
    PRIVATE
        -O2 -g
)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(ddr_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(ddr_test sdram gpio riscv)
