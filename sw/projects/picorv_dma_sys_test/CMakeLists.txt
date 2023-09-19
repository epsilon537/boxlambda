add_executable(picorv_dma_sys_test
	EXCLUDE_FROM_ALL
	main.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

add_dependencies(picorv_dma_sys_test picorv_wordcopy)

target_compile_definitions(picorv_dma_sys_test
	PRIVATE
		-DCONFIG_CPU_NOP="nop"
)

target_compile_options(picorv_dma_sys_test
	PRIVATE
		-O2
)

#Not sure if this is needed.
target_include_directories(picorv_dma_sys_test
	PRIVATE
    	${CMAKE_CURRENT_BINARY_DIR}
)

target_include_directories(picorv_dma_sys_test
    PRIVATE
	${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(picorv_dma_sys_test ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)

target_link_libraries(picorv_dma_sys_test picorv_dma)

