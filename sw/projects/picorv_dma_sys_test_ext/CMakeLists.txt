#PicoRV_DMA system test program

add_executable(picorv_dma_sys_test_ext
	EXCLUDE_FROM_ALL
	picorv_dma_sys_test_ext.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

#Dependency on two PicoRV test assembly programs
add_dependencies(
	picorv_dma_sys_test_ext 
	picorv_wordcopy_single 
	picorv_bytecopy_single 
	picorv_wordcopy_burst
	picorv_bytecopy_burst)

target_compile_options(picorv_dma_sys_test_ext
	PRIVATE
		-O2 -g
)

target_include_directories(picorv_dma_sys_test_ext
    PRIVATE
		${CMAKE_CURRENT_BINARY_DIR}
		${CMAKE_BINARY_DIR}/sw/components/picorv_dma/test/
)

link_internal_create_mem_file(picorv_dma_sys_test_ext 
	${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)

target_link_libraries(picorv_dma_sys_test_ext picorv_dma vera sdram)
