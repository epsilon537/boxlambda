add_executable(sdspi_test
 EXCLUDE_FROM_ALL
 sdspi_test_main.c
 sdtest.c
 ../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_compile_definitions(sdspi_test
 PRIVATE
  -DSDSPI_CPU_IS_LITTLE_ENDIAN
)

target_include_directories(sdspi_test
    PRIVATE
     ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)

#Function defined in parent CMakeLists.txt file:
link_and_create_mem_file(sdspi_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_cmem_boot.ld)

target_link_libraries(sdspi_test gpio)
