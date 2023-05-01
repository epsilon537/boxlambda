add_executable(sdspi_test_hw
	EXCLUDE_FROM_ALL
	sdspi_test_hw.c
	sdtest.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7-35/sw/libs/soc/utils.c
)

target_compile_definitions(sdspi_test_hw
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

target_include_directories(sdspi_test_hw
    PRIVATE
    ${PROJECT_SOURCE_DIR}/sub/sdspi/sw/
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(sdspi_test_hw)
