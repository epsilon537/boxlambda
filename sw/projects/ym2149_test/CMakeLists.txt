add_executable(ym2149_test
	EXCLUDE_FROM_ALL
	main.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

target_compile_definitions(ym2149_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(ym2149_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
