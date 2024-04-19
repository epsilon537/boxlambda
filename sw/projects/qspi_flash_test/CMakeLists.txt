#Flash test program

add_executable(qspi_flash_test
	EXCLUDE_FROM_ALL
	qspi_flash_test.cpp
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_compile_options(qspi_flash_test
	PRIVATE
		-O2 -g
)

link_internal_create_mem_file(qspi_flash_test
	${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_flash_mem.ld)

target_link_libraries(qspi_flash_test flashdrvr sdram)
