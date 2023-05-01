add_executable(sdspi_test_ver
	EXCLUDE_FROM_ALL
	sdspi_test.c
	sdspi_hal.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_compile_definitions(sdspi_test_ver
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(sdspi_test_ver)
