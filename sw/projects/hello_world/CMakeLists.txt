add_executable(hello_world
	EXCLUDE_FROM_ALL
    hello.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/gpio.c
	../../../sub/ibex_wb/soc/fpga/arty-a7/sw/libs/soc/utils.c
)

#Setting the -g flag for the hello_dbg build testing GDB access.
target_compile_options(hello_world
	PRIVATE -g)

#Function defined in parent CMakeLists.txt file:
link_internal_create_mem_file(hello_world ${PROJECT_SOURCE_DIR}/sw/components/bootstrap/link_internal_mem_64K.ld)
