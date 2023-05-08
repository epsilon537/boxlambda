add_executable(sdspi_test
	EXCLUDE_FROM_ALL
	main.c
	sdtest.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_compile_definitions(sdspi_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
		-DSDSPI_CPU_ENDIANNESS=LITTLE_ENDIAN
)

target_include_directories(sdspi_test
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(sdspi_test)
