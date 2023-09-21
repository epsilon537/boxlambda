add_executable(ddr_test
	EXCLUDE_FROM_ALL
	ddr_test.c
    memtest.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

if(CMAKE_BUILD_TYPE STREQUAL "sim")
	target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=32768)
else() #fpga:
	target_compile_definitions(ddr_test PRIVATE -DMEMTEST_SIZE=\(256*1024*1024\))
endif()

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(ddr_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)

target_link_libraries(ddr_test sdram)